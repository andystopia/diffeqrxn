{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module AST
  ( ExportAST (..),
    fixExportAST,
    MacroProvider (..),
    chemicalNames,
    exportASTGroupMaybeASCII,
    basicMacroProvider,
    ResolvedVariableModel (..),
    speciesNormalFormToExportAST,
    lintEquation,
    qualifyResolvedVariables,
    semanticErrorEffToEither,
    exportASTGetStateVars,
    resolveEquations,
    simpleExportAST,
    exportASTGetParameters,
    equationsToNormalForm,
    EquationNormalFormDef,
  )
where

import Control.Monad (forM_, unless, when)
import Data.Bifunctor (second, Bifunctor (bimap))
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.Functor (void, (<&>))
import Data.List (group,  sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Set as S
import Effectful (Eff, (:>))
import qualified Error.Diagnose as Diag
import ErrorProvider
import Parser
  ( EqnSide (..),
    Equation (..),
    MacroArg (..),
    MacroArgValue (..),
    Parameter,
    RxnControl (..),
    RxnControlDirection (..),
    StateVariable,
    functionArgToParameter,
    functionArgToStateVariable, ModelVariableDeclaration (..),
  )
import Utils (Span, Spannable (..), Spanned (..), joinWithAnd, unspanned)
import Effectful.Error.Dynamic (Error)
import Data.Function ((&))

data ExportAST
  = ExportSum [ExportAST]
  | ExportNeg ExportAST
  | ExportDiv ExportAST ExportAST
  | ExportPow ExportAST ExportAST
  | ExportProd [ExportAST]
  | ExportParameter Parameter
  | ExportStateVar StateVariable
  | ExportInt Int
  deriving (Show)

fixExportAST :: (ExportAST -> ExportAST) -> ExportAST -> ExportAST
fixExportAST f (ExportSum els) = ExportSum (f <$> els)
fixExportAST f (ExportNeg els) = ExportNeg (f els)
fixExportAST f (ExportDiv num denom) = ExportDiv (f num) (f denom)
fixExportAST f (ExportPow a b) = ExportPow (f a) (f b)
fixExportAST f (ExportProd els) = ExportSum (f <$> els)
-- base cases
fixExportAST _ lit@(ExportParameter _) = lit
fixExportAST _ lit@(ExportStateVar _) = lit
fixExportAST _ lit@(ExportInt _) = lit

-- | essentially is a first step towards bubbling
-- | negative values up in the tree
rewriteProductOfNegativeToNegativeProduct :: ExportAST -> ExportAST
-- this is a rule which transposes products and negatives, removing negatives
-- if possible
rewriteProductOfNegativeToNegativeProduct (ExportProd values) =
  let rewritten = rewriteProductOfNegativeToNegativeProduct <$> values
      negativesRemoved =
        ( \case
            ExportNeg v -> v
            v -> v
        )
          <$> rewritten
      negativesRemovedCount =
        length $
          filter
            ( \case
                ExportNeg _ -> True
                _ -> False
            )
            rewritten
   in if negativesRemovedCount `mod` 2 == 1
        then ExportNeg (ExportProd negativesRemoved)
        else ExportProd negativesRemoved
-- remove a negative of a negative whenever they occur
rewriteProductOfNegativeToNegativeProduct (ExportNeg (ExportNeg a)) = a
-- let's remove all negative sums
-- SUM[NEG[...], NEG[...], ...] -> Neg[SUM[...]]
rewriteProductOfNegativeToNegativeProduct (ExportSum values) =
  let rewritten = rewriteProductOfNegativeToNegativeProduct <$> values

      negativesRemoved =
        ( \case
            ExportNeg v -> v
            v -> v
        )
          <$> rewritten
      negativesRemovedAll =
        all
          ( \case
              ExportNeg _ -> True
              _ -> False
          )
          rewritten
   in if negativesRemovedAll
        then (ExportNeg . ExportSum) negativesRemoved
        else ExportSum rewritten
-- and then recurse otherwise
rewriteProductOfNegativeToNegativeProduct v = fixExportAST rewriteProductOfNegativeToNegativeProduct v

fixExportASTMonoid :: (Monoid a) => (ExportAST -> a) -> ExportAST -> a
fixExportASTMonoid f (ExportSum els) = foldMap f els
fixExportASTMonoid f (ExportNeg els) = f els
fixExportASTMonoid f (ExportDiv num denom) = f num <> f denom
fixExportASTMonoid f (ExportPow a b) = f a <> f b
fixExportASTMonoid f (ExportProd els) = foldMap f els
-- \| you should almost definitely catch these base cases
-- \| but otherwise you'll just keep getting the mempty out
fixExportASTMonoid _ (ExportParameter _) = mempty
fixExportASTMonoid _ (ExportStateVar _) = mempty
fixExportASTMonoid _ (ExportInt _) = mempty

exportASTGetStateVars :: ExportAST -> S.Set StateVariable
exportASTGetStateVars (ExportStateVar var) = S.singleton var
exportASTGetStateVars ast = fixExportASTMonoid exportASTGetStateVars ast

exportASTGetParameters :: ExportAST -> S.Set Parameter
exportASTGetParameters (ExportParameter var) = S.singleton var
exportASTGetParameters ast = fixExportASTMonoid exportASTGetParameters ast

exportASTOperatorImportance :: ExportAST -> Int
exportASTOperatorImportance (ExportSum _) = 1
exportASTOperatorImportance (ExportNeg _) = 2
exportASTOperatorImportance (ExportDiv _ _) = 2
exportASTOperatorImportance (ExportProd _) = 2
exportASTOperatorImportance (ExportPow _ _) = 3
exportASTOperatorImportance (ExportParameter _) = 1000
exportASTOperatorImportance (ExportStateVar _) = 1000
exportASTOperatorImportance (ExportInt _) = 1000

-- I think the rule is if we have encoded a PROD[a, SUM[b, c]]
-- then we have a nested importance which is less than the outer importance
-- that is the PROD is saying that a should bind tighter to b, than to c,
-- but the structure is saying that b and c is more binding than a and b.
-- so I think the solution is that if the outer power is greater than the inner power
-- than we need to wrap in parenthesis.

exportApply :: (ExportAST -> ExportAST) -> ExportAST -> ExportAST
exportApply f (ExportSum value) = ExportSum (f <$> value)
exportApply f (ExportProd value) = ExportProd (f <$> value)
exportApply f (ExportNeg ext) = ExportNeg (f ext)
exportApply _ e = e

exportIsEmpty :: ExportAST -> Bool
exportIsEmpty (ExportSum []) = True
exportIsEmpty (ExportProd []) = True
exportIsEmpty _ = False

simplifyExportAST :: ExportAST -> ExportAST
simplifyExportAST (ExportSum [value]) = simplifyExportAST value
simplifyExportAST (ExportProd [value]) = simplifyExportAST value
simplifyExportAST (ExportNeg (ExportNeg value)) = simplifyExportAST value
simplifyExportAST everything = exportApply simplifyExportAST everything

simpleExportAST :: ExportAST -> ExportAST
simpleExportAST = rewriteProductOfNegativeToNegativeProduct . removeEmptyExportAST . simplifyExportAST

removeEmptyExportAST :: ExportAST -> ExportAST
removeEmptyExportAST (ExportSum value) = ExportSum (filter (not . exportIsEmpty) value)
removeEmptyExportAST (ExportProd value) = ExportProd (filter (not . exportIsEmpty) value)
removeEmptyExportAST everything = exportApply simplifyExportAST everything

-- | Extract the chemical names from a single equation
chemicalNames :: Equation -> S.Set StateVariable
chemicalNames (Equation (Chemicals left) (Chemicals right) _) = S.fromList (unspanned <$> left) <> S.fromList (unspanned <$> right)
chemicalNames (Equation (Chemicals left) _ _) = S.fromList (unspanned <$> left)
chemicalNames (Equation _ (Chemicals right) _) = S.fromList (unspanned <$> right)
chemicalNames _ = S.empty

chemicalNamesAsList :: Equation -> [StateVariable]
chemicalNamesAsList (Equation (Chemicals left) (Chemicals right) _) = (unspanned <$> left) <> (unspanned <$> right)
chemicalNamesAsList (Equation (Chemicals left) _ _) = unspanned <$> left
chemicalNamesAsList (Equation _ (Chemicals right) _) = unspanned <$> right
chemicalNamesAsList _ = mempty

data SpeciesChange a = SpeciesCreated a | SpeciesLost a deriving (Show)

data EquationNormalFormDef
  = -- | we're either increasing (proportional to) by a given macro
    SpeciesMacro {_rates :: [SpeciesChange Parameter], funcName :: Spanned String, argumentSpan :: Span, macroPositionalArguments :: [MacroArgValue], macroNamedArguments :: M.Map String MacroArgValue}
  | -- | or (proportionally) due to the product of the sum of rates and the product of chemicals (concentrations)
    SpeciesChemicals {_rates :: [SpeciesChange Parameter], chemicals :: [StateVariable]}
  | -- | or we're just increasing / decreasing our derivative by a constant
    SpeciesUnit {_rates :: [SpeciesChange Parameter]}
  deriving (Show)

-- a <==> a + b {kf, kr} -> a => a + b {kr} and a <= a + b {kf}
-- in the first case: a is on the right side so it is formed at a rate of kr * a
-- however in the second case it's on both sides again, so we run into -kf * (b * a)
-- these are incompatible with each other, so
-- data Equation

-- rules for transforming chemical equations to differential equations.
-- 1. Species on the right hand side of a forward equation are made at a rate the (sum of the rate constants) times (product of the left hand species)
-- 2. Species on the left hand side of a forward equation are lost at a rate of the (sum of the rate constants) times (product of the left hand species)

argumentsToPositionalAndNamed :: [MacroArg] -> ([MacroArgValue], M.Map String MacroArgValue)
argumentsToPositionalAndNamed args =
  second
    M.fromList
    ( partitionEithers
        ( fmap
            ( \case
                Named argName value -> Right (unspanned argName, value)
                Positional value -> Left value
            )
            args
        )
    )

equationSpeciesToNormalForm :: StateVariable -> Equation -> [EquationNormalFormDef]
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Chemicals rchems) cntrl) =
  case cntrl of
    DirectionalRxn ForwardRxn forwardRates -> forwardRxn (unspanned <$> forwardRates)
    DirectionalRxn BackwardsRxn backwardRates -> backwardRxn (unspanned <$> backwardRates)
    BothRxn forwardRates backwardRates -> forwardRxn (unspanned <$> forwardRates) ++ backwardRxn (unspanned <$> backwardRates)
  where
    handleForwardReaction :: StateVariable -> [StateVariable] -> [StateVariable] -> [Parameter] -> [EquationNormalFormDef]
    handleForwardReaction ch lch rch rxnRates =
      let whenOnLeft = if ch `elem` lch then Just (SpeciesChemicals (SpeciesLost <$> rxnRates) lch) else Nothing
          whenOnRight = if ch `elem` rch then Just (SpeciesChemicals (SpeciesCreated <$> rxnRates) lch) else Nothing
       in maybeToList whenOnLeft ++ maybeToList whenOnRight

    forwardRxn = handleForwardReaction chem (unspanned <$> lchems) (unspanned <$> rchems)
    backwardRxn = handleForwardReaction chem (unspanned <$> rchems) (unspanned <$> lchems)
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Macro name argSpan args) cntrl) =
  if chem `elem` (unspanned <$> lchems)
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> [forwardRxn forwardRates]
      DirectionalRxn BackwardsRxn backwardRates -> [backwardRxn backwardRates]
      BothRxn forwardRates backwardRates -> [forwardRxn forwardRates, backwardRxn backwardRates]
    else []
  where
    forwardRxn rxnRates = SpeciesMacro (SpeciesLost . unspanned <$> rxnRates) name argSpan posArgs namedArgs
    backwardRxn rxnRates = SpeciesMacro (SpeciesCreated . unspanned <$> rxnRates) name argSpan posArgs namedArgs
    (posArgs, namedArgs) = argumentsToPositionalAndNamed args
equationSpeciesToNormalForm chem (Equation (Macro name argSpan args) (Chemicals rchems) cntrl) =
  if chem `elem` (unspanned <$> rchems)
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> [forwardRxn forwardRates]
      DirectionalRxn BackwardsRxn backwardRates -> [backwardRxn backwardRates]
      BothRxn forwardRates backwardRates -> [forwardRxn forwardRates, backwardRxn backwardRates]
    else []
  where
    forwardRxn rxnRates = SpeciesMacro (SpeciesCreated . unspanned <$> rxnRates) name argSpan posArgs namedArgs
    backwardRxn rxnRates = SpeciesMacro (SpeciesLost . unspanned <$> rxnRates) name argSpan posArgs namedArgs
    (posArgs, namedArgs) = argumentsToPositionalAndNamed args
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (EqnVoids _) cntrl) =
  if chem `elem` (unspanned <$> lchems)
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> forwardRxn forwardRates
      DirectionalRxn BackwardsRxn backwardRates -> backwardRxn backwardRates
      BothRxn forwardRates backwardRates -> forwardRxn forwardRates ++ backwardRxn backwardRates
    else []
  where
    forwardRxn rxnRates = [SpeciesChemicals (SpeciesLost . unspanned <$> rxnRates) (unspanned <$> lchems)]
    backwardRxn rxnRates = [SpeciesUnit (SpeciesCreated . unspanned <$> rxnRates)]
-- no op operation
equationSpeciesToNormalForm _ (Equation (EqnVoids _) (EqnVoids _) _) = []
equationSpeciesToNormalForm chem (Equation (EqnVoids _) (Chemicals rchems) cntrl) =
  if chem `elem` (unspanned <$> rchems)
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> forwardRxn forwardRates
      DirectionalRxn BackwardsRxn backwardRates -> backwardRxn backwardRates
      BothRxn forwardRates backwardRates -> forwardRxn forwardRates ++ backwardRxn backwardRates
    else []
  where
    forwardRxn rxnRates = [SpeciesUnit (SpeciesCreated . unspanned <$> rxnRates)]
    backwardRxn rxnRates = [SpeciesChemicals (SpeciesLost . unspanned <$> rxnRates) (unspanned <$> rchems)]

-- I'm pretty sure these future cases aren't able to be encountered in normal flow,
-- since each equation only reduces itself, and we don't call on functions where it's
-- known that a species doesn't exist, so this is probably not possible to encounter at all.
equationSpeciesToNormalForm _ (Equation (Macro {}) (EqnVoids _) _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation (EqnVoids _) (Macro {}) _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation (Macro {}) (Macro {}) _) = error "Two macros on opposite sides of the equation are not supported"

speciesChangeToAST :: SpeciesChange Parameter -> ExportAST
speciesChangeToAST (SpeciesCreated species) = ExportParameter species
speciesChangeToAST (SpeciesLost species) = ExportNeg (ExportParameter species)

normalFormToAST :: (SemanticErrorEff :> es) => MacroProvider -> EquationNormalFormDef -> Eff es ExportAST
normalFormToAST _ (SpeciesUnit rxnRates) = pure $ ExportSum (speciesChangeToAST <$> rxnRates)
normalFormToAST _ (SpeciesChemicals rates chems) = pure $ ExportProd [ExportSum (speciesChangeToAST <$> rates), ExportProd (ExportStateVar <$> chems)]
normalFormToAST me (SpeciesMacro rates func argSpan posArgs namedArgs) = do
  macroEval <- executeMacro me func argSpan posArgs namedArgs
  pure $ ExportProd [ExportSum (speciesChangeToAST <$> rates), macroEval]

speciesNormalFormToExportAST :: (SemanticErrorEff :> es) => MacroProvider -> [EquationNormalFormDef] -> Eff es (Either (Diag.Diagnostic String) ExportAST)
speciesNormalFormToExportAST mp forms = do
  allChildren <- traverse (semSubscope <$> normalFormToAST mp) forms

  pure $ do
    let (failures, successes) = partitionEithers allChildren

    unless (null failures) $ do
      void $ Left (fold failures)

    return $ ExportSum successes

data MacroTypes = MacroRateConstantTy | MacroChemicalTy | MacroRateConstantListTy | MacroChemicalListTy deriving (Show, Eq)

data MacroTypeDefinition = MacroTypeDefinition
  { macroNamedParameters :: M.Map String MacroTypes,
    macroPositionalParameters :: [(String, MacroTypes)]
  }

data MacroProvider = MacroProvider
  { macroTypeDecls :: M.Map String MacroTypeDefinition,
    macroEvaluators :: M.Map String ([MacroArgValue] -> M.Map String MacroArgValue -> ExportAST)
  }

executeMacro :: (SemanticErrorEff :> es) => MacroProvider -> Spanned String -> Span -> [MacroArgValue] -> M.Map String MacroArgValue -> (Eff es) ExportAST
executeMacro macroProvider (Spanned nameSpan name) argSpan positionArgs namedArguments = do
  (MacroTypeDefinition namedP positionP) <- semLiftMaybe (M.lookup name (macroTypeDecls macroProvider)) $ do
    semSetErrorCode "MACRO_SIG_NOT_FOUND"
    semSetErrorMessage $ "Missing function definition: " <> name
    semAddMarker nameSpan $ "The macro named `" <> name <> "` was not found in scope"

  evalProvider <- semLiftMaybe (M.lookup name (macroEvaluators macroProvider)) $ do
    semSetErrorCode "MACRO_DEF_NOT_FOUND"
    semSetErrorMessage $ "No macro named " <> name <> "was found in scope."
    semAddMarker nameSpan "This function was not found in scope"

  -- let's validate that we have enough, and then we're going to validate
  -- that we have matching types
  when (length positionP /= length positionArgs) $ do
    semBuildError $ do
      semSetErrorCode "FUNC_POS_ARITY_ERR"
      semSetErrorMessage $ name <> " expects that " <> show (length positionP) <> " positional arguments will be passed, but " <> (show . length) positionArgs <> " were passed"
      semAddMarker argSpan "incorrect number of arguments passed here"
      semAddNote $ "The position arguments to " <> name <> " are as follows: " <> joinWithAnd (fst <$> positionP)
      semAddHint "Making a positional argument named or vice versa is not supported."

  -- now we're going to typecheck the positional parameters.
  -- TODO: if type inference is added, then dont' erase the inner types while checking this.

  let typecheck desiredType actualArg =
        case actualArg of
          OnceArg _ | (desiredType /= MacroRateConstantTy) && (desiredType /= MacroChemicalTy) -> do
            semBuildError $ do
              semSetErrorCode "MACRO_ARG_TYPE_INCOMPATIBILITY"
              semSetErrorMessage "Macro argument had an incorrect type passed"
              semAddMarker (computeSpan actualArg) "A single rate constant or chemical was passed, but a list of chemicals or rate constants is required in this position"
              semAddHint "Pass a list of rate constants (for example: [R1, R2]) or a list of chemicals (for example: [NaCL, NaOH])"
          ListArg _
            | (desiredType /= MacroRateConstantListTy) && (desiredType /= MacroChemicalListTy) -> do
                semBuildError $ do
                  semSetErrorCode "MACRO_ARG_TYPE_INCOMPATIBILITY"
                  semSetErrorMessage "Macro argument had an incorrect type passed"
                  semAddMarker (computeSpan actualArg) "A list was passed but a single rate constant or chemical species was expected"
          _ -> pure ()
  forM_ (zip (snd <$> positionP) positionArgs) $ uncurry typecheck
  -- now we want to move onto the named parameters, since I believe those will be tricky as well.

  let namedPParamNames = M.keysSet namedP
  let namedArgNames = M.keysSet namedArguments

  let (inArgs, inBoth, inParams) = (S.difference namedArgNames namedPParamNames, M.intersectionWithKey (const (,)) namedP namedArguments, S.difference namedPParamNames namedArgNames)

  unless (null inParams) $ do
    semBuildError $ do
      semSetErrorCode "MACRO_NAMED_PARAMETER_MISSING"
      semSetErrorMessage $ "Missing named arguments to macro " <> name
      if S.size inParams == 1
        then semAddMarker argSpan ("Missing argument named `" <> joinWithAnd (S.toList inParams) <> "` whilst calling " <> name)
        else semAddMarker argSpan ("Missing " <> joinWithAnd (S.toList inParams) <> " arguments to " <> name)

  unless (null inArgs) $
    semBuildError $ do
      semSetErrorCode "MACRO_UNKNOWN_NAMED_ARGUMENT"
      semSetErrorMessage $ "Unknown parameters passed to " <> name
      -- TODO: point to the actual argument not all of them
      semAddMarker argSpan ("Unknown parameters" <> joinWithAnd (S.toList inArgs))

  -- let's do a typecheck on the rest.
  forM_ (M.elems inBoth) $ uncurry typecheck

  -- commit the errors and return if anything has gone wrong
  -- since we never want to call the function with anything other
  -- than the types that it has asked for.
  semCommitIfErrs

  -- let collectedResList = uncurry argTypeCheck <$> zip (snd <$> positionP) positionArgs

  return $ evalProvider positionArgs namedArguments

hillSumMacro :: [StateVariable] -> Parameter -> Parameter -> ExportAST
hillSumMacro chems rate n =
  let powAST = ExportPow (ExportSum (ExportStateVar <$> chems)) (ExportParameter n)
   in ExportDiv powAST (ExportSum [ExportPow (ExportParameter rate) (ExportParameter n), powAST])

-- todo: once the AST has spanning information, we'll
-- replace this with a real typechecking abstraction that reports
-- errors on the AST
hillSumWrapper :: [MacroArgValue] -> M.Map String MacroArgValue -> ExportAST
hillSumWrapper positional named =
  case positional of
    [ListArg chems] ->
      if M.size named == 2
        then
          let both = do
                rate <- M.lookup "rate" named
                n <- M.lookup "n" named
                return (rate, n)
           in case both of
                -- TODO: I'm pretty sure that my SEM handler can
                -- pretty accurately capture the case where this goes
                -- wrong, so I think if we worked with that, we wouldn't
                -- need this atrocious hack.
                Just (OnceArg rate, OnceArg n) -> hillSumMacro (functionArgToStateVariable . unspanned <$> chems) (fromJust (functionArgToParameter (unspanned rate))) (fromJust (functionArgToParameter (unspanned n)))
                _ -> error "hillsum not called with correct rate and n parameters"
        else error "HillSum called with the wrong number of named parameters"
    _ -> error "This is an error"

evaluateMacro :: MacroProvider -> String -> [MacroArgValue] -> M.Map String MacroArgValue -> ExportAST
evaluateMacro _ name positional namedArgs = if name == "HillSum" then hillSumWrapper positional namedArgs else error "only the hillsum function is supported"

equationToNormalForm :: Equation -> M.Map StateVariable [EquationNormalFormDef]
equationToNormalForm eqn =
  let names = chemicalNamesAsList eqn
      unified = (group . sort) names
      unifiedNormals = (fmap . concatMap) (`equationSpeciesToNormalForm` eqn) unified
   in M.fromList (zip (head <$> unified) unifiedNormals)

equationsToNormalForm :: [Equation] -> M.Map StateVariable [EquationNormalFormDef]
equationsToNormalForm = M.unionsWith (<>) . fmap equationToNormalForm

exportAstGroupMaybe :: (ExportAST -> String) -> (ExportAST -> String) -> ExportAST -> ExportAST -> String
exportAstGroupMaybe whenNotGrouped whenGrouped parentAstNode astNode =
  let outerPrec = exportASTOperatorImportance parentAstNode
      innerPrec = exportASTOperatorImportance astNode
   in if innerPrec < outerPrec
        then whenGrouped astNode
        else whenNotGrouped astNode

exportNormalForms :: (SemanticErrorEff :> es, Error (Diag.Diagnostic String) :> es) => MacroProvider -> (String -> ExportAST -> String) -> M.Map String [EquationNormalFormDef] -> Eff es (Either (Diag.Diagnostic String) [String])
exportNormalForms mp exporter normalForms = do
  forms <- traverse (uncurry convertForm) (M.toList normalForms)

  pure $ do
    res <- foldEitherLeft forms
    return $ uncurry exporter <$> res
  where
    convertForm species nf = do
      nfExport <- speciesNormalFormToExportAST mp nf
      pure $ second ((species,) . simpleExportAST) nfExport




data ResolvedVariableModel = ResolvedVariableModel
  { resolvedStateVariables :: S.Set StateVariable,
    resolvedSystemParameters :: S.Set Parameter,
    computedSystemParameters :: S.Set Parameter,
    implicitSystemParameters :: S.Set Parameter,
    -- | "dependent variables" are nodes which have
    -- | no dependencies.
    resolvedDependentStateVariables :: S.Set StateVariable,
    resolvedEquations :: M.Map StateVariable ExportAST
  }
  deriving (Show)

resolvedModelFromEquations :: M.Map StateVariable ExportAST -> ResolvedVariableModel
resolvedModelFromEquations eqns =
  let exportASTs = M.elems eqns
      independentStateVars = foldMap exportASTGetStateVars exportASTs
      stateVars = S.fromList (M.keys eqns)
      paramVars = foldMap exportASTGetParameters exportASTs
   in ResolvedVariableModel
        { resolvedStateVariables = stateVars,
          resolvedSystemParameters = paramVars,
          computedSystemParameters = S.empty,
          implicitSystemParameters = S.empty,
          resolvedDependentStateVariables = stateVars S.\\ independentStateVars,
          resolvedEquations = eqns
        }

qualifyResolvedVariables :: [ModelVariableDeclaration] -> ResolvedVariableModel -> ResolvedVariableModel
qualifyResolvedVariables decls model =
  let (implicit, computed) =
        decls
          <&> ( \x -> case x of
                  ImplicitInModel _ var -> Left $ unspanned var
                  ComputedInModel _ var -> Right $ unspanned var
              )
          & partitionEithers
          & bimap S.fromList S.fromList
   in model
        { computedSystemParameters = computed,
          implicitSystemParameters = implicit
        }


lintEquation :: (SemanticErrorEff :> es) => Equation -> (Eff es) Equation
lintEquation (Equation macro1@(Macro {}) macro2@(Macro {}) _) = do
  semBuildError $ do
    semSetErrorCode "TWO_SIDED_MACRO"
    semSetErrorMessage "Macros are only allowed one side of chemical reactions"
    semAddMarker (computeSpan macro1) "This macro..."
    semAddMarker (computeSpan macro2) "... and this macro cannot be on both sides"
    semMakeIntoWarning
  semCommit
lintEquation (Equation (EqnVoids sp1) (EqnVoids sp2) _) = do
  semBuildError $ do
    semSetErrorCode "TWO_SIDED_VOID"
    semSetErrorMessage "A void expression on both sides is functionally a no-op, and is not supported."
    semAddMarker sp1 "This void..."
    semAddMarker sp2 "... and this void cannot be on opposite sides"
    semMakeIntoWarning
  semCommit
lintEquation eq = return eq


foldEitherLeft :: (Monoid b) => [Either b a] -> Either b [a]
foldEitherLeft input = do
  let (errors, successes) = partitionEithers input

  if null errors
    then Right successes
    else Left (fold errors)

foldHashmapLeft :: (Monoid e, Ord k) => M.Map k (Either e a) -> Either e (M.Map k a)
foldHashmapLeft hashmap = do
  let (keys, values) = unzip (M.toList hashmap)
  foldedLeft <- foldEitherLeft values
  let outputMap = M.fromList (zip keys foldedLeft)
  return outputMap

resolveEquations ::
  (SemanticErrorEff :> es) =>
  MacroProvider ->
  M.Map StateVariable [EquationNormalFormDef] ->
  Eff es ResolvedVariableModel
resolveEquations macroProvider normalForms = do
  converted <- traverse (speciesNormalFormToExportAST macroProvider) normalForms
  equationResolved <- semLiftEitherDiag $ foldHashmapLeft converted
  return $ resolvedModelFromEquations equationResolved

exportASTGroupMaybeASCII :: (ExportAST -> String) -> ExportAST -> ExportAST -> String
exportASTGroupMaybeASCII converter = exportAstGroupMaybe converter (\inner -> "(" <> converter inner <> ")")

basicMacroProvider :: MacroProvider
basicMacroProvider =
  MacroProvider
    ( M.fromList
        [ ( "HillSum",
            MacroTypeDefinition
              (M.fromList [("rate", MacroRateConstantTy), ("n", MacroRateConstantTy)])
              [("chemicals", MacroChemicalListTy)]
          )
        ]
    )
    (M.fromList [("HillSum", hillSumWrapper)])

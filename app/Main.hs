{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Main where

import qualified Cli
import qualified CodeWriter as CW
import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Data.Bifunctor (first, second)
import Data.Either (partitionEithers, lefts)
import Data.Foldable (fold, forM_, sequenceA_, traverse_)
import Data.List (group, intercalate, sort)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Traversable (forM)
import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError, tryError)
import Effectful.State.Static.Local (evalState, execState, gets, modify)
import qualified Error.Diagnose as Diag
import qualified JSONExporter as JE
import Lexer
import System.IO (stderr)
import Text.Earley hiding (namedToken)
import Prelude hiding (lex)

-- newtype Chemical = Chemical String deriving (Show)

data EqnSide = Chemicals [Spanned String] | EqnVoids Span | Function (Spanned String) Span [FunctionArg] deriving (Show)

instance Spannable EqnSide where
  -- span :: a -> Span
  computeSpan (Chemicals strs) = foldr1 (<>) (computeSpan <$> strs)
  computeSpan (EqnVoids sp) = sp
  computeSpan (Function name argSpan _args) = computeSpan name <> argSpan

data RxnDirection = BiRxn | Forwards | Backwards deriving (Show)

data FunctionArg = Named (Spanned String) FunctionArgValue | Positional FunctionArgValue deriving (Show)

instance Spannable FunctionArg where
  computeSpan (Named name values) = computeSpan name <> computeSpan values
  computeSpan (Positional value) = computeSpan value

data FunctionArgValue = ListArg [Spanned String] | OnceArg (Spanned String) deriving (Show)

instance Spannable FunctionArgValue where
  computeSpan (ListArg els) = foldr1 (<>) (computeSpan <$> els)
  computeSpan (OnceArg val) = computeSpan val

data RxnControlDirection = ForwardRxn | BackwardsRxn deriving (Show)

data RxnControl = DirectionalRxn RxnControlDirection [Spanned String] | BothRxn [Spanned String] [Spanned String] deriving (Show)

data Equation = Equation
  { lhs :: EqnSide,
    rhs :: EqnSide,
    control :: RxnControl
  }
  deriving (Show)

-- | lexer definition (evaluator)
expr :: Grammar r (Prod r String (Spanned Token) Equation)
expr = mdo
  rxn <- rule $ (chemicalRxnBi <|> chemicalRxnUni) <?> "a complete chemical reaction"

  chemicalRxnBi <- rule $ Equation <$> eqnSide <*> (bidir *> eqnSide) <*> biRxnConstants <?> "bidireciton chemical reaction"
  chemicalRxnUni <-
    rule
      $ (\left op right consts -> Equation left right (DirectionalRxn op consts))
        <$> eqnSide
        <*> unidir
        <*> eqnSide
        <*> uniRxnConstants
      <?> "unidirectional rate constants"
  eqnSide <- rule $ Chemicals <$> chemicals <|> ((EqnVoids <$> tokSpan VoidKw) <?> "one side of a chemical equation") <|> function
  chemicals <- rule $ ((:) <$> singleChem <* (tok' AddOp <?> "+ (for more chemicals)") <*> chemicals) <|> pure <$> singleChem <?> "a list of chemicals"

  unidir <-
    rule $
      ForwardRxn <$ tok ForwardsOp
        <|> BackwardsRxn <$ tok BackwardsOp

  bidir <-
    rule $
      BiRxn <$ tok BiOp <?>
        "<==>"

  biRxnConstants <-
    rule
      $ BothRxn
        <$> ( tok LBrace
                *> (rateConstantsList <* (tok Comma <?> "a comma separator between rate constants"))
            )
        <*> (rateConstantsList <* tok RBrace)
      <?> "rate constants (for example {f1, f2})"

  uniRxnConstants <- rule $ tok LBrace *> rateConstantsList <* tok RBrace <?> "rate constants"

  rateConstantsList <- rule $ (:) <$> rateConstant <* tok AddOp <*> rateConstantsList <|> pure <$> rateConstant <?> "rate constants"

  singleChem <- rule $ terminal asIdentS <?> "a chemical"
  rateConstant <- rule $ terminal asIdentS <?> "rate constant"

  -- function parsing
  funcArgLit <- rule $ terminal asIdentS <?> "chemical or rate constant"
  functionName <- rule $ terminal asIdentS <?> "the name of a function"
  parameterName <- rule $ terminal asIdentS <?> "parameter name"

  arrayValue <- rule $ (:) <$> funcArgLit <* (tok' Comma <?> "a comma to introduce more list elements") <*> arrayValue <|> pure <$> funcArgLit
  arrayValue' <- rule $ tok' LBrac *> arrayValue <* tok' RBrac <?> "list of chemicals & rate constants (i.e, [X, Y])"

  argumentValue <- rule $ (ListArg <$> arrayValue') <|> (OnceArg <$> funcArgLit)
  kwValue <- rule $ Named <$> parameterName <* tok EqOp <*> argumentValue

  argument <- rule $ kwValue <|> (Positional <$> argumentValue)

  argumentList <- rule $ (:) <$> argument <* (tok' Comma <?> "a comma to introduce more function arguments") <*> argumentList <|> pure <$> argument

  argumentList' <- rule $ (\l m r -> (l <> r, m)) <$> tokSpan LParen <*> argumentList <*> tokSpan RParen <?> "function arguments"

  function <- rule $ (\name (argspan, args) -> Function name argspan args) <$> functionName <*> argumentList'

  return rxn
  where
    asIdentS :: Spanned Token -> Maybe (Spanned String)
    asIdentS (Spanned sp unTok) = sequenceA $ Spanned sp (asIdent unTok)

    tokSpan :: Token -> Prod r String (Spanned Token) Span
    tokSpan x = computeSpan <$> tok x

    tok :: Token -> Prod r String (Spanned Token) (Spanned Token)
    tok x = satisfy (\v -> x == unspanned v) <?> tokToSimple x
    tok' :: Token -> Prod r String (Spanned Token) (Spanned Token)
    tok' x = satisfy (\v -> x == unspanned v)

-- instance Diag.HasHints Void String where
--   hints _ = []

filterRepetitions :: (a -> a -> Bool) -> [a] -> [a]
filterRepetitions _ [] = []
filterRepetitions _ [x] = [x]
filterRepetitions cond (a : b : rest) | cond a b = filterRepetitions cond (a : rest)
filterRepetitions cond (a : b : rest) = a : filterRepetitions cond (b : rest)

filterRepeatedValue :: (Eq a) => (a -> Bool) -> [a] -> [a]
filterRepeatedValue cond = filterRepetitions (\a b -> a == b && cond a)

joinWithOr :: [String] -> String
joinWithOr [] = ""
joinWithOr [a] = a
joinWithOr [a, b] = a <> " or " <> b
joinWithOr [a, b, c] = a <> ", " <> b <> ", or " <> c
joinWithOr (a : rest) = a <> ", " <> joinWithOr rest

joinWithAnd :: [String] -> String
joinWithAnd [] = ""
joinWithAnd [a] = a
joinWithAnd [a, b] = a <> " and " <> b
joinWithAnd [a, b, c] = a <> ", " <> b <> ", and " <> c
joinWithAnd (a : rest) = a <> ", " <> joinWithAnd rest

-- runErrorHandler :: Diag.Diagnostic String ->

type instance DispatchOf SemanticErrorEff = Dynamic

type instance DispatchOf SemanticErrorBuilder = Dynamic

data SemanticErrorBuilder :: Effect where
  SemSetErrorCode :: String -> SemanticErrorBuilder m ()
  SemSetErrorMessage :: String -> SemanticErrorBuilder m ()
  SemAddMarker :: Span -> Diag.Marker String -> SemanticErrorBuilder m ()
  SemAddHint :: Diag.Note String -> SemanticErrorBuilder m ()
  SemMakeIntoWarning :: SemanticErrorBuilder m ()
  SemMakeError :: SemanticErrorBuilder m ()

semSetErrorCode :: (SemanticErrorBuilder :> es) => String -> (Eff es) ()
semSetErrorCode x = send (SemSetErrorCode x)

semSetErrorMessage :: (SemanticErrorBuilder :> es) => String -> (Eff es) ()
semSetErrorMessage x = send (SemSetErrorMessage x)

semAddMarker :: (SemanticErrorBuilder :> es) => Span -> Diag.Marker String -> (Eff es) ()
semAddMarker sp x = send (SemAddMarker sp x)

semAddHint :: (SemanticErrorBuilder :> es) => Diag.Note String -> (Eff es) ()
semAddHint x = send (SemAddHint x)

semMakeIntoWarning :: (SemanticErrorBuilder :> es) => (Eff es) ()
semMakeIntoWarning = send SemMakeIntoWarning

semMakeError :: (SemanticErrorBuilder :> es) => (Eff es) ()
semMakeError = send SemMakeError

data SemErrorBuild = SemErrorBuild
  { _errCode :: Maybe String,
    _errMessage :: String,
    _errMarkers :: [(Span, Diag.Marker String)],
    _notes :: [Diag.Note String],
    _isWarning :: Bool
  }

defaultSemErrorBuild :: SemErrorBuild
defaultSemErrorBuild = SemErrorBuild Nothing "<an unknown error occurred>" [] [] False

-- we need to be able to have the file that we're currently focused on
-- to actually build this model.

spanToPosition :: String -> Span -> Diag.Position
spanToPosition filepath (Span spanLine spanStart spanEnd) = Diag.Position (spanLine, spanStart) (spanLine, spanEnd) filepath

semErrorBuildIntoReport :: String -> SemErrorBuild -> Diag.Report String
semErrorBuildIntoReport filepath (SemErrorBuild code msg markers notes isWarn) = (if isWarn then Diag.Warn else Diag.Err) code msg (first (spanToPosition filepath) <$> markers) notes

runSemanticErrorBuilder :: Eff (SemanticErrorBuilder : es) a -> (Eff es) SemErrorBuild
runSemanticErrorBuilder = reinterpret (execState defaultSemErrorBuild) $ \_ -> \case
  SemSetErrorCode x -> do
    modify (\c -> c {_errCode = Just x})
    return ()
  SemSetErrorMessage x -> do
    modify (\c -> c {_errMessage = x})
    return ()
  SemAddMarker sp msg -> do
    currentMarkers <- gets _errMarkers
    let withAdditional = currentMarkers ++ [(sp, msg)]
    modify (\c -> c {_errMarkers = withAdditional})
    return ()
  SemAddHint msg -> do
    currentHints <- gets _notes
    let withAdditional = currentHints ++ [msg]
    modify (\c -> c {_notes = withAdditional})
    return ()
  SemMakeIntoWarning -> do
    modify (\c -> c {_isWarning = True})
    return ()
  SemMakeError -> do
    modify (\c -> c {_isWarning = False})
    return ()

data SemanticErrorEffData = SemanticErrorEffData
  { diagnostic :: Diag.Diagnostic String,
    semCurrentFile :: String,
    semErrCount :: Int,
    semCurrentFiles :: [(String, String)]
  }

data SemanticErrorEff :: Effect where
  SemPushError :: Diag.Report String -> SemanticErrorEff m ()
  SemPushDiagnostic :: Diag.Diagnostic String -> SemanticErrorEff m ()
  SemBuildError :: Eff '[SemanticErrorBuilder] () -> SemanticErrorEff m ()
  SemAddFile :: String -> String -> SemanticErrorEff m ()
  SemActivateFile :: String -> SemanticErrorEff m ()
  SemSubscope :: Eff '[SemanticErrorEff, Error (Diag.Diagnostic String)] a -> SemanticErrorEff m (Either (Diag.Diagnostic String) a)
  SemCommitIfErrs :: SemanticErrorEff m (Maybe a)
  SemCommit :: SemanticErrorEff m a

runSemanticErrorEff :: (Error (Diag.Diagnostic String) :> es) => Eff (SemanticErrorEff : es) a -> (Eff es) a
runSemanticErrorEff = do
  let addReport repo = modify (\c -> c {diagnostic = diagnostic c `Diag.addReport` repo, semErrCount = semErrCount c + 1})

  reinterpret (evalState (SemanticErrorEffData mempty "" 0 [])) $ \_ -> \case
    SemPushError err -> do
      void $ addReport err
    SemBuildError err -> do
      currentFile <- gets semCurrentFile
      let err' = semErrorBuildIntoReport currentFile ((runPureEff . runSemanticErrorBuilder) err)
      void $ addReport err'
    SemAddFile file contents -> do
      void $ modify (\c -> c {diagnostic = Diag.addFile (diagnostic c) file contents, semCurrentFiles = semCurrentFiles c ++ [(file, contents)]})
    SemActivateFile file -> do
      void $ modify (\c -> c {semCurrentFile = file})
    SemCommit -> do
      current <- gets diagnostic
      throwError current
    SemSubscope eff -> do
      files <- gets semCurrentFiles
      curFile <- gets semCurrentFile
      let res = runPureEff . runErrorNoCallStack @(Diag.Diagnostic String) . runSemanticErrorEff $ do
            forM_ files $ \(filepath, filename) -> do
              semAddFile filepath filename
            semActivateFile curFile
            eff
      return res
    SemPushDiagnostic diag -> do
      forM_ (Diag.reportsOf diag) $ \repo -> do
        void $ addReport repo
    SemCommitIfErrs -> do
      current <- gets diagnostic
      nErrs <- gets semErrCount

      if nErrs /= 0
        then throwError current
        else pure Nothing

-- throwError current

semSubscope :: (SemanticErrorEff :> es) => Eff '[SemanticErrorEff, Error (Diag.Diagnostic String)] a -> (Eff es) (Either (Diag.Diagnostic String) a)
semSubscope = send . SemSubscope

-- semSubscopeErr :: (Error (Diag.Diagnostic String) :> es) => Eff es c -> Eff es (Either (Diag.Diagnostic String) c)
-- semSubscopeErr subscope = do
--   res <- tryError subscope
--   pure (first snd res)

semLiftMaybe :: (SemanticErrorEff :> es) => Maybe a -> Eff '[SemanticErrorBuilder] () -> Eff es a
semLiftMaybe (Just a) _ = pure a
semLiftMaybe Nothing eff = do
  semBuildError eff
  semCommit

semPushDiagnostic :: (SemanticErrorEff :> es) => Diag.Diagnostic String -> (Eff es) ()
semPushDiagnostic e = send (SemPushDiagnostic e)

semLiftEitherDiag :: (SemanticErrorEff :> es, Error (Diag.Diagnostic String) :> es) => Either (Diag.Diagnostic String) k -> (Eff es) k
semLiftEitherDiag (Right k) = return k
semLiftEitherDiag (Left e) = do
  semPushDiagnostic e
  semCommit

semPushError :: (SemanticErrorEff :> es) => Diag.Report String -> (Eff es) ()
semPushError e = send (SemPushError e)

semCommit :: (SemanticErrorEff :> es) => (Eff es) a
semCommit = send SemCommit

semAddFile :: (SemanticErrorEff :> es) => String -> String -> Eff es ()
semAddFile name contents = send (SemAddFile name contents)

semActivateFile :: (SemanticErrorEff :> es) => String -> Eff es ()
semActivateFile name = send (SemActivateFile name)

semCommitIfErrs :: (SemanticErrorEff :> es) => (Eff es) ()
semCommitIfErrs = void (send SemCommitIfErrs)

semBuildError :: (SemanticErrorEff :> es) => Eff '[SemanticErrorBuilder] () -> (Eff es) ()
semBuildError e = send (SemBuildError e)

parse :: FilePath -> String -> [[Spanned Token]] -> Either (Diag.Diagnostic String) [Equation]
parse fileName fileContents tokens = do
  let parseExpr = fullParses (parser expr)
  let parsedLines = parseExpr <$> tokens
  let (eqns, reports) = unzip parsedLines

  let badReports = filter (\v -> (not . null . expected . snd) v || (not . null . unconsumed . snd) v) (zip tokens reports)

  let parsedDiagnostic = Diag.addFile (mempty :: Diag.Diagnostic String) fileName fileContents
  repos <- forM badReports $ \(errLine, Report pos expec _) -> do
    let (linen, scol, ecol) =
          if pos == length errLine
            then
              let (Spanned (Span linen' _ ecol') _) = last errLine
               in (linen', ecol', ecol')
            else
              let (Spanned (Span linen' scol' ecol') _) = errLine !! pos
               in (linen', scol', ecol')

    let expectedMessage = if null expec then "end of line was expected to be here" else "expected " ++ joinWithOr expec

    let parseErrorMessage = if all (null . unconsumed) reports then "Incomplete Equation ~ Equation ended before expected" else "Reaction couldn't be parsed"
    let repo =
          Diag.Err
            (Just "while parsing")
            parseErrorMessage
            [ (Diag.Position (linen, scol) (linen, ecol) fileName, Diag.This expectedMessage)
            ]
            []

    return repo

  let diagnostics = foldl Diag.addReport parsedDiagnostic repos

  if null badReports
    then do
      let eqns' = head <$> eqns
      Right eqns'
    else Left diagnostics

data LiteralType = LiteralStateVar | LiteralParameter deriving (Show, Eq)

data ExportAST
  = ExportSum [ExportAST]
  | ExportNeg ExportAST
  | ExportDiv ExportAST ExportAST
  | ExportPow ExportAST ExportAST
  | ExportProd [ExportAST]
  | ExportLiteral LiteralType String
  | ExportInt Int
  deriving (Show)

fixExportAST :: (ExportAST -> ExportAST) -> ExportAST -> ExportAST
fixExportAST f (ExportSum els) = ExportSum (f <$> els)
fixExportAST f (ExportNeg els) = ExportNeg (f els)
fixExportAST f (ExportDiv num denom) = ExportDiv (f num) (f denom)
fixExportAST f (ExportPow a b) = ExportPow (f a) (f b)
fixExportAST f (ExportProd els) = ExportSum (f <$> els)
-- base cases
fixExportAST _ lit@(ExportLiteral _ _) = lit
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
fixExportASTMonoid _ (ExportLiteral _ _) = mempty
fixExportASTMonoid _ (ExportInt _) = mempty

-- retrieve all the variables which are either
-- state variables or not state variables (depending on the parameter)
-- passed from the AST
exportASTGetVariables :: LiteralType -> ExportAST -> S.Set String
exportASTGetVariables
  littype
  (ExportLiteral lit literal)
    | lit == littype =
        S.singleton literal
exportASTGetVariables lit ast = fixExportASTMonoid (exportASTGetVariables lit) ast

exportASTOperatorImportance :: ExportAST -> Int
exportASTOperatorImportance (ExportSum _) = 1
exportASTOperatorImportance (ExportNeg _) = 2
exportASTOperatorImportance (ExportDiv _ _) = 2
exportASTOperatorImportance (ExportProd _) = 2
exportASTOperatorImportance (ExportPow _ _) = 3
exportASTOperatorImportance (ExportLiteral _ _) = 1000
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

typstLit' :: String -> String
typstLit' lit = intercalate "_" (typstLitH <$> splitOn "_" lit)
  where
    typstLitH :: String -> String
    typstLitH [v] = [v]
    typstLitH x = "\"" <> x <> "\""

typstLit :: String -> String
typstLit lit = concat $ intercalateMany cycler divided
  where
    typstLitH :: String -> String
    typstLitH [v] = [v]
    typstLitH x = "\"" <> x <> "\""

    divided :: [String]
    divided = typstLitH <$> splitOn "_" lit
    cycler :: [String]
    cycler = ["_", "\\_"]

    intercalateMany :: [a] -> [a] -> [a]
    intercalateMany _ [] = []
    intercalateMany _ [a] = [a]
    intercalateMany (current : future) (a : rest) =
      [a, current] ++ intercalateMany (future ++ [current]) rest
    intercalateMany [] _ = error "empty intercalating list passed"

mapSeparateFirst :: (a -> b) -> (a -> b) -> [a] -> [b]
mapSeparateFirst _ _ [] = []
mapSeparateFirst inj _ [v] = [inj v]
mapSeparateFirst inj mapper (a : rest) = inj a : (mapper <$> rest)

exportASTToTypst :: ExportAST -> String
exportASTToTypst (ExportLiteral LiteralParameter lit) = intercalate "_" (typstLit <$> splitOn "_" lit)
exportASTToTypst (ExportLiteral LiteralStateVar lit) = typstLit lit
exportASTToTypst export@(ExportProd p) = intercalate " dot.c " $ func' <$> p
  where
    func' = exportASTGroupMaybeASCII exportASTToTypst export
exportASTToTypst x = exportUsingASCII exportASTToTypst x

intercalateStringPeriodically :: Int -> String -> [String] -> [String]
intercalateStringPeriodically maxLen inner list =
  intercalateStringPeriodicallyH 0 maxLen inner list
  where
    intercalateStringPeriodicallyH :: Int -> Int -> String -> [String] -> [String]
    intercalateStringPeriodicallyH _ _ _ [] = []
    intercalateStringPeriodicallyH _ _ _ [a] = [a]
    intercalateStringPeriodicallyH count maxLen inner (current : rest) =
      if count + length current > maxLen
        then [inner, current] ++ intercalateStringPeriodicallyH (length inner) maxLen inner rest
        else current : intercalateStringPeriodicallyH (length current + count) maxLen inner rest

exportASTToTypst' :: ExportAST -> String
exportASTToTypst' export = case export of
  (ExportSum s) ->
    concat $
      intercalateStringPeriodically 120 " \\ & " $
        intersperseCondEither
          func'
          ( \case
              ExportNeg v -> Left v
              c -> Right c
          )
          " - "
          " + "
          s
  _ -> exportASTToTypst export
  where
    func' = exportASTGroupMaybeASCII exportASTToTypst export

exportAstGroupMaybe :: (ExportAST -> String) -> (ExportAST -> String) -> ExportAST -> ExportAST -> String
exportAstGroupMaybe whenNotGrouped whenGrouped parentAstNode astNode =
  let outerPrec = exportASTOperatorImportance parentAstNode
      innerPrec = exportASTOperatorImportance astNode
   in if innerPrec < outerPrec
        then whenGrouped astNode
        else whenNotGrouped astNode

exportASTGroupMaybeASCII :: (ExportAST -> String) -> ExportAST -> ExportAST -> String
exportASTGroupMaybeASCII converter = exportAstGroupMaybe converter (\inner -> "(" <> converter inner <> ")")

intersperseCond :: (a -> Bool) -> a -> a -> [a] -> [a]
intersperseCond _ _ _ [] = []
intersperseCond _ _ _ [v] = [v]
intersperseCond cond l r (a : b : rest) = [a, if cond b then l else r] ++ intersperseCond cond l r (b : rest)

intersperseCondT :: (a -> b) -> (a -> Bool) -> b -> b -> [a] -> [b]
intersperseCondT _ _ _ _ [] = []
intersperseCondT conv _ _ _ [v] = [conv v]
intersperseCondT conv cond l r (a : b : rest) =
  [conv a, if cond b then l else r] ++ intersperseCondT conv cond l r (b : rest)

intersperseCondEither :: (a -> b) -> (a -> Either a a) -> b -> b -> [a] -> [b]
intersperseCondEither _ _ _ _ [] = []
intersperseCondEither conv _ _ _ [v] = [conv v]
intersperseCondEither conv cond l r (a : b : rest) =
  [ conv a,
    case cond b of
      Left _ -> l
      Right _ -> r
  ]
    ++ intersperseCondEither conv cond l r ((fromEither . cond) b : rest)

fromEither :: Either a a -> a
fromEither (Left v) = v
fromEither (Right v) = v

exportUsingASCII :: (ExportAST -> String) -> ExportAST -> String
exportUsingASCII func export =
  case export of
    (ExportLiteral _ str) -> str
    (ExportInt int) -> show int
    (ExportNeg str) -> "-" <> func' str
    (ExportSum s) ->
      concat $
        intersperseCondEither
          func'
          ( \case
              ExportNeg v -> Left v
              c -> Right c
          )
          " - "
          " + "
          s
    (ExportProd p) -> intercalate " * " $ func' <$> p
    (ExportPow a b) -> func' a <> "^" <> func' b
    (ExportDiv a b) -> func' a <> "/" <> func' b
  where
    -- we do this to add parantheses so that the expansion
    -- doesn't output something algebraically invalid, by leaving off
    -- parentheses.
    func' = exportASTGroupMaybeASCII func export

exportToASCII :: ExportAST -> String
exportToASCII = exportUsingASCII exportToASCII

-- | Extract the unique chemical names out of a set of equations.
-- (Note: does not currently include )
allChemicalNames :: [Equation] -> [String]
allChemicalNames eqns = Set.toList $ foldMap chemicalNames eqns

-- | Extract the chemical names from a single equation
chemicalNames :: Equation -> Set.Set String
chemicalNames (Equation (Chemicals left) (Chemicals right) _) = Set.fromList (unspanned <$> left) <> Set.fromList (unspanned <$> right)
chemicalNames (Equation (Chemicals left) _ _) = Set.fromList (unspanned <$> left)
chemicalNames (Equation _ (Chemicals right) _) = Set.fromList (unspanned <$> right)
chemicalNames _ = Set.empty

chemicalNamesAsList :: Equation -> [String]
chemicalNamesAsList (Equation (Chemicals left) (Chemicals right) _) = (unspanned <$> left) <> (unspanned <$> right)
chemicalNamesAsList (Equation (Chemicals left) _ _) = unspanned <$> left
chemicalNamesAsList (Equation _ (Chemicals right) _) = unspanned <$> right
chemicalNamesAsList _ = mempty

data SpeciesChange a = SpeciesCreated a | SpeciesLost a deriving (Show)

data EquationNormalFormDef
  = -- | we're either increasing (proportional to) by a given macro
    SpeciesMacro {_rates :: [SpeciesChange String], funcName :: Spanned String, argumentSpan :: Span, macroPositionalArguments :: [FunctionArgValue], macroNamedArguments :: M.Map String FunctionArgValue}
  | -- | or (proportionally) due to the product of the sum of rates and the product of chemicals (concentrations)
    SpeciesChemicals {_rates :: [SpeciesChange String], chemicals :: [String]}
  | -- | or we're just increasing / decreasing our derivative by a constant
    SpeciesUnit {_rates :: [SpeciesChange String]}
  deriving (Show)

-- a <==> a + b {kf, kr} -> a => a + b {kr} and a <= a + b {kf}
-- in the first case: a is on the right side so it is formed at a rate of kr * a
-- however in the second case it's on both sides again, so we run into -kf * (b * a)
-- these are incompatible with each other, so
-- data Equation

-- rules for transforming chemical equations to differential equations.
-- 1. Species on the right hand side of a forward equation are made at a rate the (sum of the rate constants) times (product of the left hand species)
-- 2. Species on the left hand side of a forward equation are lost at a rate of the (sum of the rate constants) times (product of the left hand species)

argumentsToPositionalAndNamed :: [FunctionArg] -> ([FunctionArgValue], M.Map String FunctionArgValue)
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

equationSpeciesToNormalForm :: String -> Equation -> [EquationNormalFormDef]
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Chemicals rchems) cntrl) =
  case cntrl of
    DirectionalRxn ForwardRxn forwardRates -> forwardRxn (unspanned <$> forwardRates)
    DirectionalRxn BackwardsRxn backwardRates -> backwardRxn (unspanned <$> backwardRates)
    BothRxn forwardRates backwardRates -> forwardRxn (unspanned <$> forwardRates) ++ backwardRxn (unspanned <$> backwardRates)
  where
    handleForwardReaction :: String -> [String] -> [String] -> [String] -> [EquationNormalFormDef]
    handleForwardReaction ch lch rch rxnRates =
      let whenOnLeft = if ch `elem` lch then Just (SpeciesChemicals (SpeciesLost <$> rxnRates) lch) else Nothing
          whenOnRight = if ch `elem` rch then Just (SpeciesChemicals (SpeciesCreated <$> rxnRates) lch) else Nothing
       in maybeToList whenOnLeft ++ maybeToList whenOnRight

    forwardRxn = handleForwardReaction chem (unspanned <$> lchems) (unspanned <$> rchems)
    backwardRxn = handleForwardReaction chem (unspanned <$> rchems) (unspanned <$> lchems)
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Function name argSpan args) cntrl) =
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
equationSpeciesToNormalForm chem (Equation (Function name argSpan args) (Chemicals rchems) cntrl) =
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
    forwardRxn rxnRates = [SpeciesUnit (SpeciesLost . unspanned <$> rxnRates)]
    backwardRxn rxnRates = [SpeciesChemicals (SpeciesCreated . unspanned <$> rxnRates) (unspanned <$> lchems)]
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
equationSpeciesToNormalForm _ (Equation (Function {}) (EqnVoids _) _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation (EqnVoids _) (Function {}) _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation (Function {}) (Function {}) _) = error "Two macros on opposite sides of the equation are not supported"

speciesChangeToAST :: SpeciesChange String -> ExportAST
speciesChangeToAST (SpeciesCreated species) = ExportLiteral LiteralParameter species
speciesChangeToAST (SpeciesLost species) = ExportNeg (ExportLiteral LiteralParameter species)

normalFormToAST :: (SemanticErrorEff :> es) => MacroProvider -> EquationNormalFormDef -> Eff es ExportAST
normalFormToAST _ (SpeciesUnit rxnRates) = pure $ ExportSum (speciesChangeToAST <$> rxnRates)
normalFormToAST _ (SpeciesChemicals rates chems) = pure $ ExportProd [ExportSum (speciesChangeToAST <$> rates), ExportProd (ExportLiteral LiteralStateVar <$> chems)]
normalFormToAST me (SpeciesMacro rates func argSpan posArgs namedArgs) = do
  macroEval <- executeMacro me func argSpan posArgs namedArgs
  pure $ ExportProd [ExportSum (speciesChangeToAST <$> rates), macroEval]

speciesNormalFormToExportAST :: (SemanticErrorEff :> es, Error (Diag.Diagnostic String) :> es) => MacroProvider -> [EquationNormalFormDef] -> Eff es (Either (Diag.Diagnostic String) ExportAST)
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
    macroEvaluators :: M.Map String ([FunctionArgValue] -> M.Map String FunctionArgValue -> ExportAST)
  }

executeMacro :: (SemanticErrorEff :> es) => MacroProvider -> Spanned String -> Span -> [FunctionArgValue] -> M.Map String FunctionArgValue -> (Eff es) ExportAST
executeMacro macroProvider (Spanned nameSpan name) argSpan positionArgs namedArguments = do
  (MacroTypeDefinition namedP positionP) <- semLiftMaybe (M.lookup name (macroTypeDecls macroProvider)) $ do
    semSetErrorCode "MACRO_SIG_NOT_FOUND"
    semSetErrorMessage $ "Missing function definition: " <> name
    semAddMarker nameSpan (Diag.This $ "The macro named `" <> name <> "` was not found in scope")

  evalProvider <- semLiftMaybe (M.lookup name (macroEvaluators macroProvider)) $ do
    semSetErrorCode "MACRO_DEF_NOT_FOUND"
    semSetErrorMessage $ "No macro named " <> name <> "was found in scope."
    semAddMarker nameSpan (Diag.This "This function was not found in scope")

  -- let's validate that we have enough, and then we're going to validate
  -- that we have matching types
  when (length positionP /= length positionArgs) $ do
    semBuildError $ do
      semSetErrorCode "FUNC_POS_ARITY_ERR"
      semSetErrorMessage $ name <> " expects that " <> show (length positionP) <> " positional arguments will be passed, but " <> (show . length) positionArgs <> " were passed"
      semAddMarker argSpan (Diag.This "incorrect number of arguments passed here")
      semAddHint $ Diag.Note $ "The position arguments to " <> name <> " are as follows: " <> joinWithAnd (fst <$> positionP)
      semAddHint $ Diag.Hint "Making a positional argument named or vice versa is not supported."

  -- now we're going to typecheck the positional parameters.
  -- TODO: if type inference is added, then dont' erase the inner types while checking this.

  let typecheck desiredType actualArg =
        case actualArg of
          OnceArg _ | (desiredType /= MacroRateConstantTy) && (desiredType /= MacroChemicalTy) -> do
            semBuildError $ do
              semSetErrorCode "MACRO_ARG_TYPE_INCOMPATIBILITY"
              semSetErrorMessage "Macro argument had an incorrect type passed"
              semAddMarker (computeSpan actualArg) (Diag.This "A single rate constant or chemical was passed, but a list of chemicals or rate constants is required in this position")
              semAddHint $ Diag.Hint "Pass a list of rate constants (for example: [R1, R2]) or a list of chemicals (for example: [NaCL, NaOH])"
          ListArg _
            | (desiredType /= MacroRateConstantListTy) && (desiredType /= MacroChemicalListTy) -> do
                semBuildError $ do
                  semSetErrorCode "MACRO_ARG_TYPE_INCOMPATIBILITY"
                  semSetErrorMessage "Macro argument had an incorrect type passed"
                  semAddMarker (computeSpan actualArg) (Diag.This "A list was passed but a single rate constant or chemical species was expected")
          _ -> pure ()
  forM_ (zip (snd <$> positionP) positionArgs) $ uncurry typecheck
  -- now we want to move onto the named parameters, since I believe those will be tricky as well.

  let namedPParamNames = M.keysSet namedP
  let namedArgNames = M.keysSet namedArguments

  let (inArgs, inBoth, inParams) = (Set.difference namedArgNames namedPParamNames, M.intersectionWithKey (const (,)) namedP namedArguments, Set.difference namedPParamNames namedArgNames)

  unless (null inParams) $ do
    semBuildError $ do
      semSetErrorCode "MACRO_NAMED_PARAMETER_MISSING"
      semSetErrorMessage $ "Missing named arguments to macro " <> name
      if Set.size inParams == 1
        then semAddMarker argSpan (Diag.This $ "Missing argument named `" <> joinWithAnd (Set.toList inParams) <> "` whilst calling " <> name)
        else semAddMarker argSpan (Diag.This $ "Missing " <> joinWithAnd (Set.toList inParams) <> " arguments to " <> name)

  unless (null inArgs) $
    semBuildError $ do
      semSetErrorCode "MACRO_UNKNOWN_NAMED_ARGUMENT"
      semSetErrorMessage $ "Unknown parameters passed to " <> name
      -- TODO: point to the actual argument not all of them
      semAddMarker argSpan (Diag.This $ "Unknown parameters" <> joinWithAnd (Set.toList inArgs))

  -- let's do a typecheck on the rest.
  forM_ (M.elems inBoth) $ uncurry typecheck

  -- commit the errors and return if anything has gone wrong
  -- since we never want to call the function with anything other
  -- than the types that it has asked for.
  semCommitIfErrs

  -- let collectedResList = uncurry argTypeCheck <$> zip (snd <$> positionP) positionArgs

  return $ evalProvider positionArgs namedArguments

hillSumMacro :: [String] -> String -> String -> ExportAST
hillSumMacro chems rate n =
  let powAST = ExportPow (ExportSum (ExportLiteral LiteralStateVar <$> chems)) (ExportLiteral LiteralParameter n)
   in ExportDiv powAST (ExportSum [ExportPow (ExportLiteral LiteralParameter rate) (ExportLiteral LiteralParameter n), powAST])

-- todo: once the AST has spanning information, we'll
-- replace this with a real typechecking abstraction that reports
-- errors on the AST
hillSumWrapper :: [FunctionArgValue] -> M.Map String FunctionArgValue -> ExportAST
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
                Just (OnceArg rate, OnceArg n) -> hillSumMacro (unspanned <$> chems) (unspanned rate) (unspanned n)
                _ -> error "hillsum not called with correct rate and n parameters"
        else error "HillSum called with the wrong number of named parameters"
    _ -> error "This is an error"

evaluateMacro :: MacroProvider -> String -> [FunctionArgValue] -> M.Map String FunctionArgValue -> ExportAST
evaluateMacro _ name positional namedArgs = if name == "HillSum" then hillSumWrapper positional namedArgs else error "only the hillsum function is supported"

equationToNormalForm :: Equation -> M.Map String [EquationNormalFormDef]
-- equationToNormalForm eqn = M.fromSet (`equationSpeciesToNormalForm` eqn) (chemicalNames eqn)
equationToNormalForm eqn =
  let names = chemicalNamesAsList eqn
      unified = (group . sort) names
      unifiedNormals = (fmap . concatMap) (`equationSpeciesToNormalForm` eqn) unified
   in M.fromList (zip (head <$> unified) unifiedNormals)

equationsToNormalForm :: [Equation] -> M.Map String [EquationNormalFormDef]
equationsToNormalForm = M.unionsWith (<>) . fmap equationToNormalForm

foldEitherLeft :: (Monoid b) => [Either b a] -> Either b [a]
foldEitherLeft input = do
  let (errors, successes) = partitionEithers input

  if null errors
    then Right successes
    else Left (fold errors)

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

foldHashmapLeft :: (Monoid e, Ord k) => M.Map k (Either e a) -> Either e (M.Map k a)
foldHashmapLeft hashmap = do
  let (keys, values) = unzip (M.toList hashmap)
  foldedLeft <- foldEitherLeft values
  let outputMap = M.fromList (zip keys foldedLeft)
  return outputMap

resolveEquations ::
  (SemanticErrorEff :> es, Error (Diag.Diagnostic String) :> es) =>
  MacroProvider ->
  M.Map String [EquationNormalFormDef] ->
  Eff es ResolvedVariableModel
resolveEquations macroProvider normalForms = do
  converted <- traverse (speciesNormalFormToExportAST macroProvider) normalForms
  equationResolved <- semLiftEitherDiag $ foldHashmapLeft converted
  return $ resolvedModelFromEquations equationResolved

-- typstExporter :: String -> ExportAST -> String
-- typstExporter species definition = "(dif " <> exportASTToTypst (ExportLiteral LiteralStateVar species) <> ")/(dif t) &= " <> exportASTToTypst definition <> "\\"

typstExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (Eff es) ()
typstExporter _ (ResolvedVariableModel _stateVars _sysParams _dependent eqns) = do
  CW.scoped "$" $ do
    forM_ (M.toList eqns) $ \(species, definition) -> do
      CW.push $ "(dif " <> exportASTToTypst (ExportLiteral LiteralStateVar species) <> ")/(dif t) &= " <> (exportASTToTypst' . simpleExportAST) definition <> "\\"

  CW.push "$"
  return ()

asciiExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (Eff es) ()
asciiExporter _ (ResolvedVariableModel _stateVars _sysParams _dependent eqns) = do
  forM_ (M.toList eqns) $ \(species, definition) -> do
    CW.push $ species <> " = " <> (exportToASCII . simpleExportAST) definition

juliaExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (Eff es) ()
juliaExporter modelName (ResolvedVariableModel stateVars sysParams _dependent eqns) = do
  let stateVarList = S.toList stateVars
  let sysParamsList = S.toList sysParams

  let paramStructName = modelName <> "Params"
  let stateStructName = modelName <> "StateVars"

  CW.scoped ("@Base.kwdef struct " <> paramStructName) $ do
    forM_ sysParamsList $ \var -> do
      CW.push var

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{Vector}, s::" <> paramStructName <> ")") $ do
    CW.push $ "return " <> "[" ++ intercalate ", " (("s." <>) <$> sysParamsList) ++ "]"

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{" <> paramStructName <> "}, fromVec::Vector)") $ do
    CW.push $ "@assert length(fromVec) == " <> show (length sysParamsList)
    CW.push $ "return " <> paramStructName <> "("

    CW.indented $ do
      forM_ (zip sysParamsList [1 :: Int ..]) $ \(var, idx) -> do
        CW.push $ var <> " = " <> "fromVec[" <> show idx <> "],"

    CW.push ")"

  CW.push "end"

  CW.newline

  CW.scoped ("@Base.kwdef struct " <> stateStructName) $ do
    forM_ stateVarList $ \var -> do
      CW.push var

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{Vector}, s::" <> stateStructName <> ")") $ do
    CW.push $ "return " <> "[" ++ intercalate ", " (("s." <>) <$> stateVarList) ++ "]"

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{" <> stateStructName <> "}, fromVec::Vector)") $ do
    CW.push $ "@assert length(fromVec) == " <> show (length stateVars)
    CW.push $ "return " <> stateStructName <> "("

    CW.indented $ do
      forM_ (zip stateVarList [1 :: Int ..]) $ \(var, idx) -> do
        CW.push $ var <> " = " <> "fromVec[" <> show idx <> "],"

    CW.push ")"

  CW.push "end"

  CW.newline

  -- I believe in Julia this is a valid definition for a differential equation
  -- because the type annotations will coerce the types into what you want them to be.
  CW.scoped ("function d" <> modelName <> "(sv, " <> "p::" <> paramStructName <> ", t)::Vector") $ do
    CW.push ("s::" <> stateStructName <> " = " <> "sv")
    forM_ stateVarList $ \stateVar -> do
      CW.push ("d" <> stateVar <> " = " <> (exportASTToJulia . simpleExportAST $ (eqns M.! stateVar)))

    CW.push $ "return " <> "[" ++ intercalate ", " (("d" <>) <$> stateVarList) ++ "]"

  CW.push "end"

  return ()

data ResolvedVariableModel = ResolvedVariableModel
  { resolvedStateVariables :: S.Set String,
    resolvedSystemParameters :: S.Set String,
    -- | "dependent variables" are nodes which have
    -- | no dependencies.
    resolvedDependentStateVariables :: S.Set String,
    resolvedEquations :: M.Map String ExportAST
  }
  deriving (Show)

resolvedModelFromEquations :: M.Map String ExportAST -> ResolvedVariableModel
resolvedModelFromEquations eqns =
  let exportASTs = M.elems eqns
      independentStateVars = foldMap (exportASTGetVariables LiteralStateVar) exportASTs
      stateVars = S.fromList (M.keys eqns)
      paramVars = foldMap (exportASTGetVariables LiteralParameter) exportASTs
   in ResolvedVariableModel
        { resolvedStateVariables = stateVars,
          resolvedSystemParameters = paramVars,
          resolvedDependentStateVariables = stateVars S.\\ independentStateVars,
          resolvedEquations = eqns
        }

exportASTToJulia :: ExportAST -> String
exportASTToJulia (ExportLiteral LiteralParameter lit) = "p." <> lit
exportASTToJulia (ExportLiteral LiteralStateVar lit) = "s." <> lit
exportASTToJulia x = exportUsingASCII exportASTToJulia x

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

jsonExport :: String -> ResolvedVariableModel -> String
jsonExport modelName (ResolvedVariableModel stateVars sysParams _dependent _eqns) = JE.toString $ JE.fromResolved modelName (typstLit <$> S.toList stateVars) (typstLit <$> S.toList sysParams)

effEither :: (Error e :> es) => Either e a -> Eff es a
effEither (Left diag) = throwError diag
effEither (Right a) = return a

lintEquation :: (SemanticErrorEff :> es) => Equation -> (Eff es) Equation
lintEquation (Equation func1@(Function {}) func2@(Function {}) _) = do
  semBuildError $ do
    semSetErrorCode "TWO_SIDED_MACRO"
    semSetErrorMessage "Macros are only allowed one side of chemical reactions"
    semAddMarker (computeSpan func1) (Diag.This "This macro...")
    semAddMarker (computeSpan func2) (Diag.This "... and this macro cannot be on both sides")
    semMakeIntoWarning
  semCommit
lintEquation (Equation (EqnVoids sp1) (EqnVoids sp2) _) = do
  semBuildError $ do
    semSetErrorCode "TWO_SIDED_VOID"
    semSetErrorMessage "A void expression on both sides is functionally a no-op, and is not supported."
    semAddMarker sp1 (Diag.This "This void...")
    semAddMarker sp2 (Diag.This "... and this void cannot be on opposite sides")
    semMakeIntoWarning
  semCommit
lintEquation eq = return eq

semanticErrorEffToEither :: Eff '[SemanticErrorEff, Error (Diag.Diagnostic String), IOE] a -> IO (Either (Diag.Diagnostic String) a)
semanticErrorEffToEither = runEff . runErrorNoCallStack @(Diag.Diagnostic String) . runSemanticErrorEff

main :: IO ()
main = do
  opt <- Cli.cli

  case opt of
    Cli.SubcommandExport (Cli.ExportOpts filePath modelName exportFormat) -> do
      -- read in the file that we're loading from
      fileContents <- readFile filePath

      -- now run the error recording effect monad
      res <- semanticErrorEffToEither $ do
        -- add the file that we have
        semAddFile filePath fileContents
        -- and let the state know that this is the file that
        -- we're working with
        semActivateFile filePath

        -- TODO: this is a bit of an leaky abstraction
        -- that is using the Diagnostic type as our error
        -- type, but this is nice and linear to read this way
        -- so that is at least nice

        -- now lex and then parse our equations file, of course, failing
        -- if either fails

        modelEquations <- effEither $ do
          lexed <- lex filePath fileContents
          parse filePath fileContents lexed


        lints <- fold . lefts <$> traverse (semSubscope . lintEquation) modelEquations

        Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle lints


        -- res <- traverse (semSubscopeErr . lintEquation) modelEquations

        -- lint the equations

        -- traverse_ lintEquation modelEquations
        -- convert them into "normal form"
        let normalForms = equationsToNormalForm modelEquations

        -- take the equations that are in normal form
        -- and resolve and expand their macros.
        resolved <- resolveEquations basicMacroProvider normalForms

        -- just a convenience helper function for running a model exporter
        let runExporter model = CW.evalStringCodeWriter 4 (model modelName resolved)

        return
          ( case exportFormat of
              Cli.JSONExport -> jsonExport modelName resolved
              Cli.AsciiExport -> runExporter asciiExporter
              Cli.JuliaExport -> runExporter juliaExporter
              Cli.TypstExport -> runExporter typstExporter
          )
      case res of
        Left err -> do
          void $ Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle err
        Right success ->
          liftIO $ putStrLn success

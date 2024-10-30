{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( 
    parse,
    parseLine,
    MacroArgValue (..),
    RxnControlDirection (..),
    RxnControl (..),
    Equation (..),
    ModelVariableDeclaration (..),
    Parameter (..),
    StateVariable (..),
    EqnSide (..),
    RxnDirection (..),
    MacroArg (..),
    functionArgToParameter,
    functionArgToStateVariable,
  )
where

import Control.Applicative ((<|>))
import Lexer
import Text.Earley
import ErrorProvider
import Effectful ((:>), Eff)
import Control.Monad (forM_)
import Utils (joinWithOr, Spanned (Spanned), Span, Spannable (computeSpan), sequenceSpan, unspanned)
import Data.Either (partitionEithers)
import Data.Tuple (swap)


newtype Parameter = Parameter String deriving (Show, Ord, Eq)

data StateVariable = Monomer String | Polymer [String] deriving (Show, Ord, Eq)

data EqnSide = Chemicals [Spanned StateVariable] | EqnVoids Span | Macro (Spanned String) Span [MacroArg] deriving (Show)

instance Spannable EqnSide where
  computeSpan (Chemicals strs) = foldr1 (<>) (computeSpan <$> strs)
  computeSpan (EqnVoids sp) = sp
  computeSpan (Macro name argSpan _args) = computeSpan name <> argSpan

data RxnDirection = BiRxn | Forwards | Backwards deriving (Show)

data MacroArg = Named (Spanned String) MacroArgValue | Positional MacroArgValue deriving (Show)

instance Spannable MacroArg where
  computeSpan (Named name values) = computeSpan name <> computeSpan values
  computeSpan (Positional value) = computeSpan value

data MacroArgDynamicVariable = MacroArgEitherMonomerOrParameter String | MacroArgPolymer [String] deriving (Show)

functionArgToStateVariable :: MacroArgDynamicVariable -> StateVariable
functionArgToStateVariable (MacroArgEitherMonomerOrParameter arg) = Monomer arg
functionArgToStateVariable (MacroArgPolymer arg) = Polymer arg

functionArgToParameter :: MacroArgDynamicVariable -> Maybe Parameter
functionArgToParameter (MacroArgEitherMonomerOrParameter arg) = Just $ Parameter arg
functionArgToParameter (MacroArgPolymer _) = Nothing

data MacroArgValue = ListArg [Spanned MacroArgDynamicVariable] | OnceArg (Spanned MacroArgDynamicVariable) deriving (Show)

instance Spannable MacroArgValue where
  computeSpan (ListArg els) = foldr1 (<>) (computeSpan <$> els)
  computeSpan (OnceArg val) = computeSpan val

data RxnControlDirection = ForwardRxn | BackwardsRxn deriving (Show)

data RxnControl = DirectionalRxn RxnControlDirection [Spanned Parameter] | BothRxn [Spanned Parameter] [Spanned Parameter] deriving (Show)

data Equation = Equation
  { lhs :: EqnSide,
    rhs :: EqnSide,
    control :: RxnControl
  }
  deriving (Show)

data ModelVariableDeclaration = ImplicitInModel Span (Spanned Parameter) | ComputedInModel Span (Spanned Parameter)

instance Spannable ModelVariableDeclaration where
  computeSpan (ImplicitInModel sp var) = sp <> computeSpan var
  computeSpan (ComputedInModel sp var) = sp <> computeSpan var

parseChemical :: Grammar r (Prod r String (Spanned Token) (Spanned StateVariable))
parseChemical = mdo
  nameLit <- rule $ terminal asIdentS <?> "chemical"
  merLit <- rule $ (:) <$> (nameLit <* tok' MerBinding) <*> merLit <|> pure <$> nameLit

  return (merLitToStateVariable <$> merLit)
  where
    merLitToStateVariable :: [Spanned String] -> Spanned StateVariable
    merLitToStateVariable [monomer] = Monomer <$> monomer
    merLitToStateVariable poly = Polymer <$> sequenceSpan poly

parseMacroArgValue :: Grammar r (Prod r String (Spanned Token) (Spanned MacroArgDynamicVariable))
parseMacroArgValue = mdo
  nameLit <- rule $ terminal asIdentS <?> "chemical"
  merLit <- rule $ (:) <$> (nameLit <* tok' MerBinding) <*> merLit <|> pure <$> nameLit

  return (merLitToStateVariable <$> merLit)
  where
    merLitToStateVariable :: [Spanned String] -> Spanned MacroArgDynamicVariable
    merLitToStateVariable [monomer] = MacroArgEitherMonomerOrParameter <$> monomer
    merLitToStateVariable [] = error "somehow we parsed an empty chemical"
    merLitToStateVariable poly = MacroArgPolymer <$> sequenceSpan poly


macroGrammar :: Grammar r (Prod r String (Spanned Token) EqnSide)
macroGrammar = mdo
  chemicalOrParam <- parseMacroArgValue

  -- function parsing
  functionName <- rule $ terminal asIdentS <?> "the name of a function"
  parameterName <- rule $ terminal asIdentS <?> "parameter name"

  arrayValue <- rule $ (:) <$> chemicalOrParam <* (tok' Comma <?> "a comma to introduce more list elements") <*> arrayValue <|> pure <$> chemicalOrParam
  arrayValue' <- rule $ tok' LBrac *> arrayValue <* tok' RBrac <?> "list of chemicals & rate constants (i.e, [X, Y])"

  argumentValue <- rule $ (ListArg <$> arrayValue') <|> (OnceArg <$> chemicalOrParam)
  kwValue <- rule $ Named <$> parameterName <* tok EqOp <*> argumentValue

  argument <- rule $ kwValue <|> (Positional <$> argumentValue)

  argumentList <- rule $ (:) <$> argument <* (tok' Comma <?> "a comma to introduce more function arguments") <*> argumentList <|> pure <$> argument

  argumentList' <- rule $ (\l m r -> (l <> r, m)) <$> tokSpan LParen <*> argumentList <*> tokSpan RParen <?> "function arguments"

  function <- rule $ (\name (argspan, args) -> Macro name argspan args) <$> functionName <*> argumentList'
  return function

asIdentS :: Spanned Token -> Maybe (Spanned String)
asIdentS (Spanned sp unTok) = sequenceA $ Spanned sp (asIdent unTok)


parseLine :: Grammar r (Prod r String (Spanned Token) (Either Equation ModelVariableDeclaration))
parseLine = mdo  
  rxnOrVar <- rule (Right <$> implicit_rule <|> Right <$> explicit_rule <|> Left <$> rxn)
  rxn <- rule $ (chemicalRxnBi <|> chemicalRxnUni) <?> "a complete chemical reaction"

  implicit_rule <- rule $ ImplicitInModel <$> tokSpan ImplicitKw <*> rateConstant <?> "implicit variable declaration"
  explicit_rule <- rule $ ComputedInModel <$> tokSpan ComputedKw <*> rateConstant <?> "computed variable declaration"

  chemicalRxnBi <- rule $ Equation <$> eqnSide <*> (bidir *> eqnSide) <*> biRxnConstants <?> "bidirectional chemical reaction"
  chemicalRxnUni <-
    rule
      $ (\left op right consts -> Equation left right (DirectionalRxn op consts))
        <$> eqnSide
        <*> unidir
        <*> eqnSide
        <*> uniRxnConstants
      <?> "unidirectional rate constants"
  eqnSide <- rule $ Chemicals <$> chemicals <|> ((EqnVoids <$> tokSpan VoidKw) <?> "one side of a chemical equation") <|> macro
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

  macro <- macroGrammar

  rateConstantsList <- rule $ (:) <$> rateConstant <* tok AddOp <*> rateConstantsList <|> pure <$> rateConstant <?> "rate constants"

  rateConstant :: (Prod r String (Spanned Token) (Spanned Parameter)) <- rule $ terminal (asParameter . asIdentS) <?> "rate constant"
  singleChem <- parseChemical

  return rxnOrVar
  where
    asParameter :: Maybe (Spanned String) -> Maybe (Spanned Parameter)
    asParameter ident = do
      v <- ident
      return (Parameter <$> v)

tokSpan :: Token -> Prod r String (Spanned Token) Span
tokSpan x = computeSpan <$> tok x

tok :: Token -> Prod r String (Spanned Token) (Spanned Token)
tok x = satisfy (\v -> x == unspanned v) <?> tokToSimple x

tok' :: Token -> Prod r String (Spanned Token) (Spanned Token)
tok' x = satisfy (\v -> x == unspanned v)


parse :: (SemanticErrorEff :> es) => [[Spanned Token]] -> (Eff es) ([ModelVariableDeclaration], [Equation])
parse tokens = do
  let parseExpr = fullParses (parser parseLine)
  let parsedLines = parseExpr <$> tokens
  let (eqns, reports) = unzip parsedLines

  -- so we're parsing line by line, and
  -- what's happening here is we're zipping each line of
  -- tokens, with each line of reports.
  let badReports = filter (\v -> (not . null . expected . snd) v || (not . null . unconsumed . snd) v) (zip tokens reports)

  forM_ badReports $ \(errLine, Report pos expec _) -> do
    let expectedMessage = if null expec then "end of line was expected to be here" else "expected " ++ joinWithOr expec
    let parseErrorMessage = if all (null . unconsumed) reports then "Incomplete Equation ~ Equation ended before expected" else "Reaction couldn't be parsed"

    semBuildError $ do
      semSetErrorCode "FAILED_PARSING"
      semSetErrorMessage parseErrorMessage
      semAddMarker (computeSpan (errLine !! pos)) expectedMessage

  semCommitIfErrs

  let eqns' = head <$> eqns
  let seperated = swap (partitionEithers eqns')
  return seperated

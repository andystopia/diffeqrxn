{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (second)
import Data.Char (isAlpha, isAlphaNum)
import Data.Either (partitionEithers)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Error.Diagnose as Diag
import qualified Error.Diagnose.Compat.Megaparsec as Diag
import System.IO (stderr)
import Text.Earley hiding (namedToken)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaChar

newtype Chemical = Chemical String deriving (Show)

data EqnSide = Chemicals [String] | EqnVoids | Function String [FunctionArg] deriving (Show)

data RxnDirection = BiRxn | Forwards | Backwards deriving (Show)

data FunctionArg = Named String FunctionArgValue | Positional FunctionArgValue deriving (Show)

data FunctionArgValue = ListArg [String] | OnceArg String deriving (Show)

data RxnControlDirection = ForwardRxn | BackwardsRxn deriving (Show)

data RxnControl = DirectionalRxn RxnControlDirection [String] | BothRxn [String] [String] deriving (Show)

data Equation = Equation
  { lhs :: EqnSide,
    rhs :: EqnSide,
    control :: RxnControl
  }
  deriving (Show)

expr :: Grammar r (Prod r String Token Equation)
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
  eqnSide <- rule $ Chemicals <$> chemicals <|> ((EqnVoids <$ tok VoidKw) <?> "one side of a chemical equation") <|> function
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

  singleChem <- rule $ terminal asIdent <?> "a chemical"
  rateConstant <- rule $ terminal asIdent <?> "rate constant"

  -- function parsing
  funcArgLit <- rule $ terminal asIdent <?> "chemical or rate constant"
  functionName <- rule $ terminal asIdent <?> "the name of a function"
  parameterName <- rule $ terminal asIdent <?> "parameter name"

  arrayValue <- rule $ (:) <$> funcArgLit <* (tok' Comma <?> "a comma to introduce more list elements") <*> arrayValue <|> pure <$> funcArgLit
  arrayValue' <- rule $ tok LBrac *> arrayValue <* tok RBrac <?> "list of chemicals & rate constants (i.e, [X, Y])"

  argumentValue <- rule $ (ListArg <$> arrayValue') <|> (OnceArg <$> funcArgLit)
  kwValue <- rule $ Named <$> parameterName <* tok EqOp <*> argumentValue

  argument <- rule $ kwValue <|> (Positional <$> argumentValue)

  argumentList <- rule $ (:) <$> argument <* (tok' Comma <?> "a comma to introduce more function arguments") <*> argumentList <|> pure <$> argument

  argumentList' <- rule $ tok LParen *> argumentList <* tok RParen <?> "function arguments"

  function <- rule $ Function <$> functionName <*> argumentList'

  return rxn
  where
    tok x = satisfy (x ==) <?> tokToSimple x
    tok' x = satisfy (x ==)

data Token = Comment String | LBrace | RBrace | Ident String | ForwardsOp | BackwardsOp | BiOp | AddOp | Comma | VoidKw | LBrac | RBrac | LParen | RParen | EqOp deriving (Show, Eq)

tokToSimple :: Token -> String
tokToSimple (Comment _) = "a comment"
tokToSimple LBrace = "{"
tokToSimple RBrace = "}"
tokToSimple (Ident _) = error "never call this, always have a better name"
tokToSimple ForwardsOp = "==>"
tokToSimple BackwardsOp = "<=="
tokToSimple BiOp = "<==>"
tokToSimple AddOp = "+"
tokToSimple Comma = ","
tokToSimple VoidKw = "void"
tokToSimple LBrac = "["
tokToSimple RBrac = "]"
tokToSimple LParen = "("
tokToSimple RParen = ")"
tokToSimple EqOp = "="

data Span = Span {line :: Int, startCol :: Int, endCol :: Int} deriving (Show)

spanFromSourcePos :: Mega.SourcePos -> Mega.SourcePos -> Span
spanFromSourcePos
  (Mega.SourcePos _ sline scol)
  (Mega.SourcePos _ eline ecol)
    | sline == eline =
        Span (Mega.unPos sline) (Mega.unPos scol) (Mega.unPos ecol)
spanFromSourcePos _ _ = error "multiline span is not supported"

data Spanned a = Spanned Span a deriving (Show)

instance Functor Spanned where
  fmap f (Spanned _span value) = Spanned _span (f value)

instance Foldable Spanned where
  foldMap f (Spanned _ value) = f value

instance Traversable Spanned where
  traverse f (Spanned _span value) = Spanned _span <$> f value

unspanned :: Spanned a -> a
unspanned (Spanned _ v) = v

spanned :: LParser (a, Mega.SourcePos) -> LParser (Spanned a)
spanned megaparser = do
  start <- Mega.getSourcePos
  (x, end) <- megaparser
  pure (Spanned (spanFromSourcePos start end) x)

-- Consume whitespace following a lexeme, but record
-- its endpoint as being before the whitespace.
lexeme :: LParser a -> LParser (a, Mega.SourcePos)
lexeme megaparser = (,) <$> megaparser <*> (Mega.getSourcePos <* Mega.try MegaChar.hspace)

spannedT :: LParser a -> LParser (Spanned a)
spannedT = spanned . lexeme

asIdent :: Token -> Maybe String
asIdent (Ident contents) = Just contents
asIdent _ = Nothing

asNonTrivia :: Token -> Maybe Token
asNonTrivia (Comment _) = Nothing
asNonTrivia x = Just x

type LParser = Parsec Void String

lexer :: LParser [[Spanned Token]]
lexer = (lex_lines `Mega.sepBy` MegaChar.newline) <* Mega.eof
  where
    lex_lines = Mega.many (spannedT keywords <|> spannedT structure <|> spannedT ident <|> spannedT op <|> spannedT (Mega.try comment))
    keywords =
      Mega.choice
        [ VoidKw <$ MegaChar.string "void"
        ]
    structure =
      Mega.choice
        [ LBrace <$ MegaChar.char '{',
          Comma <$ MegaChar.char ',',
          RBrace <$ MegaChar.char '}',
          LParen <$ MegaChar.char '(',
          RParen <$ MegaChar.char ')',
          LBrac <$ MegaChar.char '[',
          RBrac <$ MegaChar.char ']'
        ]

    comment :: LParser Token
    comment = Comment <$> (Mega.try MegaChar.hspace *> MegaChar.char '#' *> Mega.manyTill (Mega.satisfy $ const True) (Mega.lookAhead $ void MegaChar.eol <|> Mega.eof))

    ident =
      ( do
          leading <- Mega.satisfy isAlpha
          trailing <- Mega.many (Mega.satisfy (\x -> isAlphaNum x || x == '_'))

          return $ Ident (leading : trailing)
      )
        Mega.<?> "chemical name or rate constant"

    op =
      Mega.choice
        [ BiOp <$ MegaChar.string "<==>",
          BackwardsOp <$ MegaChar.string "<==",
          ForwardsOp <$ MegaChar.string "==>",
          AddOp <$ MegaChar.string "+",
          EqOp <$ MegaChar.string "="
        ]

-- lexer :: String -> [String]
-- lexer [] = []
-- lexer contents = nextToken : (lexer . ignoreLeadingWS) rest
--   where

-- data LexDiag = LexDiag String;

instance Diag.HasHints Void String where
  hints _ = []

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

parseEquationText :: String -> IO [Equation]
parseEquationText fname = do
  text <- readFile fname
  case Mega.parse lexer fname text of
    Left bundle -> do
      let diag = Diag.errorDiagnosticFromBundle Nothing "File Failed To Lex" Nothing bundle
      let added = Diag.addFile diag fname text
      void $ Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle added

      return []
    Right content -> do
      let noTrivia = filter (not . null) $ catMaybes <$> (fmap . fmap) (traverse asNonTrivia) content
      let parseExpr = fullParses (parser expr)
      let parsedLines = parseExpr <$> (fmap . fmap) unspanned noTrivia
      -- pPrint content
      let (eqns, reports) = unzip parsedLines

      let badReports = filter (\v -> (not . null . expected . snd) v || (not . null . unconsumed . snd) v) (zip noTrivia reports)

      forM_ badReports $ \(errLine, Report pos expec _) -> do
        let (linen, scol, ecol) =
              if pos == length errLine
                then
                  let (Spanned (Span linen' _ ecol') _) = last errLine
                   in (linen', ecol', ecol')
                else
                  let (Spanned (Span linen' scol' ecol') _) = errLine !! pos
                   in (linen', scol', ecol')

        let diagnostic = Diag.addFile (mempty :: Diag.Diagnostic String) fname text

        let expectedMessage = if null expec then "end of line was expected to be here" else "expected " ++ joinWithOr expec
        let repo =
              Diag.Err
                Nothing
                "Reaction couldn't be parsed"
                [ (Diag.Position (linen, scol) (linen, ecol) fname, Diag.This expectedMessage)
                ]
                []

        let diag' = Diag.addReport diagnostic repo
        void $ Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle diag'

      if null badReports
        then do
          let eqns' = head <$> eqns
          return eqns'
        else return []

data ExportAST = ExportSum [ExportAST] | ExportNeg ExportAST | ExportDiv ExportAST ExportAST | ExportGrouping ExportAST | ExportPow ExportAST ExportAST | ExportProd [ExportAST] | ExportLiteral String | ExportInt Int deriving (Show)

exportApply :: (ExportAST -> ExportAST) -> ExportAST -> ExportAST
exportApply f (ExportSum value) = ExportSum (f <$> value)
exportApply f (ExportGrouping value) = ExportGrouping (f value)
exportApply f (ExportProd value) = ExportProd (f <$> value)
exportApply f (ExportNeg ext) = ExportNeg (f ext)
exportApply _ e = e

exportIsEmpty :: ExportAST -> Bool
exportIsEmpty (ExportSum []) = True
exportIsEmpty (ExportProd []) = True
exportIsEmpty _ = False

simplifyExportAST :: ExportAST -> ExportAST
simplifyExportAST (ExportGrouping (ExportSum [value])) = simplifyExportAST value
simplifyExportAST (ExportGrouping (ExportProd [value])) = simplifyExportAST value
simplifyExportAST (ExportSum [value]) = simplifyExportAST value
simplifyExportAST (ExportProd [value]) = simplifyExportAST value
simplifyExportAST everything = exportApply simplifyExportAST everything

simpleExportAST :: ExportAST -> ExportAST
simpleExportAST = removeEmptyExportAST . simplifyExportAST

removeEmptyExportAST :: ExportAST -> ExportAST
removeEmptyExportAST (ExportSum value) = ExportSum (filter (not . exportIsEmpty) value)
removeEmptyExportAST (ExportProd value) = ExportProd (filter (not . exportIsEmpty) value)
removeEmptyExportAST everything = exportApply simplifyExportAST everything

-- exportEqnSide :: String -> EqnSide -> ExportAST
-- exportEqnSide rate (Chemicals chems) = ExportProd (ExportLiteral <$> rate : chems)
-- exportEqnSide rate EqnVoids = ExportLiteral rate
-- exportEqnSide _ (Function _ _) = error "function exporting not supported yet"

-- exportMultiEqnSide :: [String] -> EqnSide -> ExportAST
-- exportMultiEqnSide many side = ExportSum $ fmap (`exportEqnSide` side) many

-- -- exportEquation :: [Equation] -> String -> ([EqnSide], [EqnSide])

-- exportEquationLeftSide :: String -> Equation -> Maybe [ExportAST]
-- exportEquationLeftSide chem (Equation (Chemicals chems) (Chemicals _) (DirectionalRxn ForwardRxn rates))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems

--           rateConstruction rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> chems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> chems)]
--           loseRates = ExportNeg . rateConstruction <$> rates
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Chemicals chems) (Chemicals rchems) (DirectionalRxn BackwardsRxn rates))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems

--           rateConstructionB rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> rchems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> rchems)]
--           loseRates = rateConstructionB <$> rates
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Chemicals chems) (Chemicals rchems) (BothRxn ratesForwards ratesBackwards))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems
--           rateConstructionB rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> rchems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> rchems)]
--           rateConstruction rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> chems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> chems)]
--           loseRates = simpleExportAST <$> ((rateConstructionB <$> ratesBackwards) ++ (ExportNeg . rateConstruction <$> ratesForwards))
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Chemicals chems) EqnVoids (BothRxn ratesForwards ratesBackwards))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems

--           rateConstructionB rate = if number == 1 then ExportProd [ExportLiteral rate] else ExportProd [ExportInt number, ExportLiteral rate]
--           rateConstruction rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> chems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> chems)]
--           loseRates = simpleExportAST <$> ((rateConstructionB <$> ratesBackwards) ++ (ExportNeg . rateConstruction <$> ratesForwards))
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Chemicals chems) EqnVoids (DirectionalRxn ForwardRxn ratesForwards))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems

--           rateConstruction rate = if number == 1 then ExportProd [ExportLiteral rate, ExportProd (ExportLiteral <$> chems)] else ExportProd [ExportInt number, ExportLiteral rate, ExportProd (ExportLiteral <$> chems)]
--           loseRates = simpleExportAST <$> (ExportNeg . rateConstruction <$> ratesForwards)
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Chemicals chems) EqnVoids (DirectionalRxn BackwardsRxn ratesBackwards))
--   | chem `elem` chems =
--       let number = length $ filter (chem ==) chems

--           rateConstructionB rate = if number == 1 then ExportProd [ExportLiteral rate] else ExportProd [ExportInt number, ExportLiteral rate]
--           loseRates = simpleExportAST <$> (rateConstructionB <$> ratesBackwards)
--        in Just loseRates
-- exportEquationLeftSide chem (Equation (Function name args) (Chemicals [chem1]) (DirectionalRxn ForwardRxn ratesForward))
--   | chem1 == chem =
--       Just [ExportProd [ExportGrouping (ExportSum (ExportLiteral <$> ratesForward)), applyMacro name args]]
-- -- here we just capture the errors and fallthrough
-- exportEquationLeftSide _ (Equation EqnVoids EqnVoids _) = error "It's nonsensical for the void to make the void"
-- exportEquationLeftSide _ _ = Nothing

-- applyMacro :: String -> [FunctionArg] -> ExportAST
-- applyMacro "HillSum" [Positional (ListArg arg), Named "rate" (OnceArg rate), Named "n" (OnceArg n)] =
--   let powAST = ExportPow (ExportGrouping (ExportSum (ExportLiteral <$> arg))) (ExportLiteral n)
--    in ExportDiv powAST (ExportGrouping (ExportSum [ExportPow (ExportLiteral rate) (ExportLiteral n), powAST]))
-- applyMacro "HillSum" args = error $ "HillSum function called with incorrect arguments: " <> show args
-- applyMacro fnName _ = error $ "unknown function" <> fnName

-- exportEquationRightSide :: String -> Equation -> Maybe [ExportAST]
-- exportEquationRightSide chem (Equation lchems rchems (DirectionalRxn ForwardRxn rates)) = exportEquationLeftSide chem (Equation rchems lchems (DirectionalRxn BackwardsRxn rates))
-- exportEquationRightSide chem (Equation lchems rchems (DirectionalRxn BackwardsRxn rates)) = exportEquationLeftSide chem (Equation rchems lchems (DirectionalRxn ForwardRxn rates))
-- exportEquationRightSide chem (Equation lchems rchems (BothRxn ratesF ratesB)) = exportEquationLeftSide chem (Equation rchems lchems (BothRxn ratesB ratesF))

-- exportEquationBothSides :: String -> Equation -> Maybe [ExportAST]
-- exportEquationBothSides chem eqn =
--   let leftSide = exportEquationLeftSide chem eqn
--       rightSide = exportEquationRightSide chem eqn
--    in case (leftSide, rightSide) of
--         (Just a, Just b) -> Just (a ++ b)
--         (Just a, Nothing) -> Just a
--         (Nothing, Just a) -> Just a
--         (Nothing, Nothing) -> Nothing

exportASTToTypst :: ExportAST -> String
exportASTToTypst (ExportLiteral lit) = intercalate "_" (typstLit <$> splitOn "_" lit)
  where
    typstLit :: String -> String
    typstLit [v] = [v]
    typstLit x = "\"" <> x <> "\""
exportASTToTypst (ExportProd p) = intercalate " dot.c " $ exportASTToTypst <$> p
exportASTToTypst x = exportUsingASCII exportASTToTypst x

exportUsingASCII :: (ExportAST -> String) -> ExportAST -> String
exportUsingASCII _ (ExportLiteral str) = str
exportUsingASCII _ (ExportInt int) = show int
exportUsingASCII func (ExportNeg str) = "-" <> func str
exportUsingASCII func (ExportSum s) = intercalate " + " $ func <$> s
exportUsingASCII func (ExportProd p) = intercalate " * " $ func <$> p
exportUsingASCII func (ExportGrouping g) = "(" <> func g <> ")"
exportUsingASCII func (ExportPow a b) = func a <> "^" <> func b
exportUsingASCII func (ExportDiv a b) = func a <> "/" <> func b

exportToASCII :: ExportAST -> String
exportToASCII = exportUsingASCII exportToASCII

-- | Extract the unique chemical names out of a set of equations.
-- (Note: does not currently include )
allChemicalNames :: [Equation] -> [String]
allChemicalNames eqns = Set.toList $ foldMap chemicalNames eqns

-- | Extract the chemical names from a single equation
chemicalNames :: Equation -> Set.Set String
chemicalNames (Equation (Chemicals left) (Chemicals right) _) = Set.fromList left <> Set.fromList right
chemicalNames (Equation (Chemicals left) _ _) = Set.fromList left
chemicalNames (Equation _ (Chemicals right) _) = Set.fromList right
chemicalNames _ = Set.empty

data SpeciesChange a = SpeciesCreated a | SpeciesLost a deriving (Show)

data EquationNormalFormDef
  = -- | we're either increasing (proportional to) by a given macro
    SpeciesMacro {_rates :: [SpeciesChange String], funcName :: String, macroPositionalArguments :: [FunctionArgValue], macroNamedArguments :: M.Map String FunctionArgValue}
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
                Named argName value -> Right (argName, value)
                Positional value -> Left value
            )
            args
        )
    )

equationSpeciesToNormalForm :: String -> Equation -> [EquationNormalFormDef]
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Chemicals rchems) cntrl) =
  case cntrl of
    DirectionalRxn ForwardRxn forwardRates -> forwardRxn forwardRates
    DirectionalRxn BackwardsRxn backwardRates -> backwardRxn backwardRates
    BothRxn forwardRates backwardRates -> forwardRxn forwardRates ++ backwardRxn backwardRates
  where
    handleForwardReaction :: String -> [String] -> [String] -> [String] -> [EquationNormalFormDef]
    handleForwardReaction ch lch rch rxnRates =
      let whenOnLeft = if ch `elem` lch then Just (SpeciesChemicals (SpeciesLost <$> rxnRates) lch) else Nothing
          whenOnRight = if ch `elem` rch then Just (SpeciesChemicals (SpeciesCreated <$> rxnRates) lch) else Nothing
       in maybeToList whenOnLeft ++ maybeToList whenOnRight

    forwardRxn = handleForwardReaction chem lchems rchems
    backwardRxn = handleForwardReaction chem rchems lchems
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) (Function name args) cntrl) =
  if chem `elem` lchems
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> [forwardRxn forwardRates]
      DirectionalRxn BackwardsRxn backwardRates -> [backwardRxn backwardRates]
      BothRxn forwardRates backwardRates -> [forwardRxn forwardRates, backwardRxn backwardRates]
    else []
  where
    forwardRxn rxnRates = SpeciesMacro (SpeciesLost <$> rxnRates) name posArgs namedArgs
    backwardRxn rxnRates = SpeciesMacro (SpeciesCreated <$> rxnRates) name posArgs namedArgs
    (posArgs, namedArgs) = argumentsToPositionalAndNamed args
equationSpeciesToNormalForm chem (Equation (Function name args) (Chemicals rchems) cntrl) =
  if chem `elem` rchems
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> [forwardRxn forwardRates]
      DirectionalRxn BackwardsRxn backwardRates -> [backwardRxn backwardRates]
      BothRxn forwardRates backwardRates -> [forwardRxn forwardRates, backwardRxn backwardRates]
    else []
  where
    forwardRxn rxnRates = SpeciesMacro (SpeciesCreated <$> rxnRates) name posArgs namedArgs
    backwardRxn rxnRates = SpeciesMacro (SpeciesLost <$> rxnRates) name posArgs namedArgs
    (posArgs, namedArgs) = argumentsToPositionalAndNamed args
equationSpeciesToNormalForm chem (Equation (Chemicals lchems) EqnVoids cntrl) =
  if chem `elem` lchems
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> forwardRxn forwardRates
      DirectionalRxn BackwardsRxn backwardRates -> backwardRxn backwardRates
      BothRxn forwardRates backwardRates -> forwardRxn forwardRates ++ backwardRxn backwardRates
    else []
  where
    forwardRxn rxnRates = [SpeciesUnit (SpeciesLost <$> rxnRates)]
    backwardRxn rxnRates = [SpeciesChemicals (SpeciesCreated <$> rxnRates) lchems]
-- no op operation
equationSpeciesToNormalForm _ (Equation EqnVoids EqnVoids _) = []
equationSpeciesToNormalForm chem (Equation EqnVoids (Chemicals rchems) cntrl) =
  if chem `elem` rchems
    then case cntrl of
      DirectionalRxn ForwardRxn forwardRates -> forwardRxn forwardRates
      DirectionalRxn BackwardsRxn backwardRates -> backwardRxn backwardRates
      BothRxn forwardRates backwardRates -> forwardRxn forwardRates ++ backwardRxn backwardRates
    else []
  where
    forwardRxn rxnRates = [SpeciesUnit (SpeciesCreated <$> rxnRates)]
    backwardRxn rxnRates = [SpeciesChemicals (SpeciesLost <$> rxnRates) rchems]
equationSpeciesToNormalForm _ (Equation (Function _ _) EqnVoids _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation EqnVoids (Function _ _) _) = error "Macros and the void cannot influence each other"
equationSpeciesToNormalForm _ (Equation (Function _ _) (Function _ _) _) = error "Two macros on opposite sides of the equation are not supported"

speciesChangeToAST :: SpeciesChange String -> ExportAST
speciesChangeToAST (SpeciesCreated species) = ExportLiteral species
speciesChangeToAST (SpeciesLost species) = ExportNeg (ExportLiteral species)

normalFormToAST :: MacroProvider -> EquationNormalFormDef -> ExportAST
normalFormToAST _ (SpeciesUnit rxnRates) = ExportSum (speciesChangeToAST <$> rxnRates)
normalFormToAST _ (SpeciesChemicals rates chems) = ExportProd [ExportSum (speciesChangeToAST <$> rates), ExportProd (ExportLiteral <$> chems)]
normalFormToAST me (SpeciesMacro rates func posArgs namedArgs) = ExportProd [ExportSum (speciesChangeToAST <$> rates), evaluateMacro me func posArgs namedArgs]

speciesNormalFormToExportAST :: MacroProvider -> [EquationNormalFormDef] -> ExportAST
speciesNormalFormToExportAST mp forms = ExportSum (normalFormToAST mp <$> forms)

-- normalFormToAST

-- normalFromToAST (SpeciesMacro rxnRates "HillSum" [Positional (ListArg chems), Named "rate" (OnceArg rate), Named "n" (OnceArg n)]) =
--   let powAST = ExportPow (ExportGrouping (ExportSum (ExportLiteral <$> chems))) (ExportLiteral n)
--    in ExportProd [ExportSum (speciesChangeToAST <$> rxnRates), ExportDiv powAST (ExportGrouping (ExportSum [ExportPow (ExportLiteral rate) (ExportLiteral n), powAST]))]

data MacroTypes = MacroRateConstantTy | MacroChemicalTy | MacroRateConstantListTy | MacroChemicalListTy

data MacroTypeDefinition = MacroTypeDefinition
  { macroNamedParameters :: M.Map String MacroTypes,
    macroPositionalParameters :: [(String, MacroTypes)]
  }

data MacroProvider = MacroProvider
  { macroTypeDecls :: M.Map String MacroTypeDefinition,
    macroEvaluators :: M.Map String ([FunctionArgValue] -> M.Map String FunctionArgValue -> ExportAST)
  }

hillSumMacro :: [String] -> String -> String -> ExportAST
hillSumMacro chems rate n =
  let powAST = ExportPow (ExportGrouping (ExportSum (ExportLiteral <$> chems))) (ExportLiteral n)
   in ExportDiv powAST (ExportGrouping (ExportSum [ExportPow (ExportLiteral rate) (ExportLiteral n), powAST]))

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
                Just (OnceArg rate, OnceArg n) -> hillSumMacro chems rate n
                _ -> error "hillsum not called with correct rate and n parameters"
        else error "HillSum called with too many named arguments"
    _ -> error "This is an error"

evaluateMacro :: MacroProvider -> String -> [FunctionArgValue] -> M.Map String FunctionArgValue -> ExportAST
evaluateMacro _ name positional namedArgs = if name == "HillSum" then hillSumWrapper positional namedArgs else error "only the hillsum function is supported"

equationToNormalForm :: Equation -> M.Map String [EquationNormalFormDef]
equationToNormalForm eqn = M.fromSet (`equationSpeciesToNormalForm` eqn) (chemicalNames eqn)

equationsToNormalForm :: [Equation] -> M.Map String [EquationNormalFormDef]
equationsToNormalForm = M.unionsWith (<>) . fmap equationToNormalForm

exportNormalForms :: MacroProvider -> (String -> ExportAST -> String) -> M.Map String [EquationNormalFormDef] -> [String]
exportNormalForms mp exporter normalForms = uncurry exportNormalForm <$> M.toList normalForms
  where
    exportNormalForm species form = exporter species (simplifyExportAST (speciesNormalFormToExportAST mp form))

typstExporter :: String -> ExportAST -> String
typstExporter species definition = "(dif " <> exportASTToTypst (ExportLiteral species) <> ")/(dif t) = " <> exportASTToTypst definition

basicMacroProvider :: MacroProvider
basicMacroProvider = MacroProvider M.empty M.empty

main :: IO ()
main = do
  -- pPrint $ lexStructureChar "{"

  eqns <- parseEquationText "models/model.rxn"
  putStrLn $ unlines (exportNormalForms basicMacroProvider typstExporter (equationsToNormalForm eqns))

-- forM_ (allChemicalNames eqns) $ \name -> do
--   -- putStr $ "(dif " <> exportASTToTypst (ExportLiteral name) <> ")/(dif t) &= "
--   -- putStr $ exportASTToTypst $ simplifyExportAST $ ExportSum $ concat $ mapMaybe (exportEquationBothSides name) eqns
--   putStr $ name <> " = "
--   putStr $ exportToASCII $ simplifyExportAST $ ExportSum $ concat $ mapMaybe (exportEquationBothSides name) eqns
--   putStrLn " \\"

-- pPrint mapped

-- pPrint

-- case Mega.parse lexer "" text of
--   Left bundle -> putStrLn (Mega.errorBundlePretty bundle)
--   Right content -> do
--     pPrint  res
--     return ()
-- let parseExpr = fullParses (parser expr)
--     let parsedLines = parseExpr <$> filter (not . null) ( mapMaybe asNonTrivia <$> content)
--     -- pPrint content
--     pPrint parsedLines
--     return ()

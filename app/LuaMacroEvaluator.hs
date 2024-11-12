{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LuaMacroEvaluator (evalRun) where

import Control.Applicative hiding (many, some)
import Control.Arrow (left)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isAlpha)
import Data.Foldable (forM_)
import qualified Data.String
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints)
import qualified Error.Diagnose.Compat.Megaparsec as Diag
import HsLua
import System.IO (stderr)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.State (initialState)
import Utils

-- So the idea that we have is we're going to parse
-- lua and lua docstrings.

-- if we encounter `---` then we know a docstring has begun
-- and we'll parse every comment after that as being part
-- of the docstring, then we'll get all the way up to the
-- function definition line, which we'll parse with megaparsec.

-- We'll parse the comment lines beginning with `@keyword`
-- and `@positional` and determine which order those
-- are passed to the lua function, and we'll take
-- the arguments as passed in from the diffeqrxn file
-- and invoke the lua function using those.

-- We're going to need to keep track of the lines which
-- we are capturing, and I think tracking the state will
-- be a bit annoying, but overall I don't expect that
-- it'll be terribly difficult in Haskell to implement this.

spanFromSourcePos :: SourcePos -> SourcePos -> Span
spanFromSourcePos
  (SourcePos _ sline scol)
  (SourcePos _ eline ecol)
    | sline == eline =
        Span (unPos sline) (unPos scol) (unPos ecol)
spanFromSourcePos _ _ = Prelude.error "multiline span is not supported"

spanned :: LParser (a, SourcePos) -> LParser (Spanned a)
spanned megaparser = do
  start <- getSourcePos
  (x, end) <- megaparser
  pure (Spanned (spanFromSourcePos start end) x)

-- Consume whitespace following a lexeme, but record
-- its endpoint as being before the whitespace.
lexemeSpan :: LParser a -> LParser (a, SourcePos)
lexemeSpan megaparser = (,) <$> megaparser <*> (getSourcePos <* Text.Megaparsec.try hspace)

spannedT :: LParser a -> LParser (Spanned a)
spannedT = spanned . lexemeSpan

data ParsedLuaFunctionSig = ParsedLuaFunction
  { luaFnName :: Spanned T.Text,
    luaFnArgs :: [Spanned T.Text]
  }
  deriving (Show)

data ParsingErrorMessage = Nil deriving (Show, Eq, Ord)

type LParser = Parsec ParsingErrorMessage T.Text

parseLuaFunction :: LParser ParsedLuaFunctionSig
parseLuaFunction = do
  void $ spannedT (string "function")
  funcName <- spannedT ident
  void $ hspace <* char '('
  args <- spannedT ident `sepBy` L.lexeme hspace (char ',')
  void $ hspace <* char ')'
  return (ParsedLuaFunction funcName args)
  where
    ident :: LParser T.Text
    ident =
      do
        leading <- T.singleton <$> alphaNumChar
        trailing <- T.pack <$> many (alphaNumChar <|> satisfy (== '_'))
        return (leading <> trailing)

instance HasHints ParsingErrorMessage msg where
  hints _ = mempty

instance ShowErrorComponent ParsingErrorMessage where
  showErrorComponent _ = Prelude.error "we don't know what to show for error components"

convertErrorBundle ::
  [Char] ->
  [Char] ->
  T.Text ->
  ParseErrorBundle T.Text ParsingErrorMessage ->
  Diag.Diagnostic T.Text
convertErrorBundle filePath fileText msg bundle = do
  let diag = Diag.errorDiagnosticFromBundle Nothing msg Nothing bundle
  let added = Diag.addFile diag filePath fileText

  added

stateAtLineNumber :: FilePath -> s -> Int -> Text.Megaparsec.State s e
stateAtLineNumber name s lineNumber =
  Text.Megaparsec.State
    { stateInput = s,
      stateOffset = 0,
      statePosState = initialPosState lineNumber name s,
      stateParseErrors = []
    }

initialPosState ::
  -- | Line number that we're starting on.
  Int ->
  -- | Name of the file the input is coming from
  FilePath ->
  -- | Input
  s ->
  PosState s
initialPosState lineNumber name s =
  PosState
    { pstateInput = s,
      pstateOffset = 0,
      pstateSourcePos = posWithLineNumber lineNumber name,
      pstateTabWidth = defaultTabWidth,
      pstateLinePrefix = ""
    }

posWithLineNumber :: Int -> FilePath -> SourcePos
posWithLineNumber lineNumber n = SourcePos n (mkPos lineNumber) pos1

doParse :: Int -> String -> T.Text -> T.Text -> Either (Diag.Diagnostic T.Text) ParsedLuaFunctionSig
doParse line name fullContents contents = do
  let bundleConvert = convertErrorBundle "<implicit-lua>" (T.unpack fullContents) "failed to parse lua function signature"
  let (_state, parseResult) = runParser' parseLuaFunction (stateAtLineNumber name contents line)
  first bundleConvert parseResult

parseLuaFunctionSigs :: [T.Text] -> [Either (Diag.Diagnostic T.Text) ParsedLuaFunctionSig]
parseLuaFunctionSigs luaLines =
  let functionLines = [(i, line) | (i, line) <- zip [1 ..] luaLines, "function" `T.isPrefixOf` line]

      allLines = T.unlines luaLines
      parseLine lineNum = doParse lineNum "<implicit-lua>" allLines
   in uncurry parseLine <$> functionLines

evalRun :: IO ()
evalRun = do

  return ()
  -- let x = parseLuaFunctionSigs ["/* this is nonsense */", "function betaNum(x y, z)", "function alphaNum(x, y, z)"]

  -- forM_ x $ \res -> do
  --   case res of
  --     Left err -> do
  --       void $ Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle err
  --     Right success ->
  --       print success
  -- run @HsLua.Exception $ do
  --   openlibs

-- -- run a script
-- void . dostring $
--   mconcat
--     [ "print(' 5! =', factorial(5), type(factorial(5)))\n",
--       "print('30! =', factorial(30), type(factorial(30)))\n"
--     ]

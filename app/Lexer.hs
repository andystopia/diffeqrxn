{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Lexer where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (catMaybes)
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints)
import qualified Error.Diagnose.Compat.Megaparsec as Diag
import Text.Megaparsec hiding (Token)
import Control.Applicative hiding (many, some)
import Text.Megaparsec.Char
import Effectful ((:>), Eff)
import ErrorProvider
import Utils (Spanned (Spanned), Span (Span))
import Prelude hiding (lex)

data Token
  = Comment String
  | LBrace
  | RBrace
  | Ident String
  | ForwardsOp
  | BackwardsOp
  | BiOp
  | AddOp
  | Comma
  | VoidKw
  | LBrac
  | RBrac
  | LParen
  | RParen
  | EqOp
  | ImplicitKw
  | MerBinding
  | ComputedKw
  deriving (Show, Eq)

tokToSimple :: Token -> String
tokToSimple (Comment _) = "a comment"
tokToSimple ImplicitKw = "'explicit'"
tokToSimple ComputedKw = "'computed'"
tokToSimple LBrace = "{"
tokToSimple RBrace = "}"
tokToSimple (Ident _) = error "never call this, always have a better name"
tokToSimple MerBinding = ":"
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

spanFromSourcePos :: SourcePos -> SourcePos -> Span
spanFromSourcePos
  (SourcePos _ sline scol)
  (SourcePos _ eline ecol)
    | sline == eline =
        Span (unPos sline) (unPos scol) (unPos ecol)
spanFromSourcePos _ _ = error "multiline span is not supported"

spanned :: LParser (a, SourcePos) -> LParser (Spanned a)
spanned megaparser = do
  start <- getSourcePos
  (x, end) <- megaparser
  pure (Spanned (spanFromSourcePos start end) x)

-- Consume whitespace following a lexeme, but record
-- its endpoint as being before the whitespace.
lexeme :: LParser a -> LParser (a, SourcePos)
lexeme megaparser = (,) <$> megaparser <*> (getSourcePos <* try hspace)

spannedT :: LParser a -> LParser (Spanned a)
spannedT = spanned . lexeme

asIdent :: Token -> Maybe String
asIdent (Ident contents) = Just contents
asIdent _ = Nothing

asNonTrivia :: Token -> Maybe Token
asNonTrivia (Comment _) = Nothing
asNonTrivia x = Just x

data ParsingErrorMessage = Nil deriving (Show, Eq, Ord);

type LParser = Parsec ParsingErrorMessage String


instance HasHints ParsingErrorMessage msg where
  hints _ = mempty

instance ShowErrorComponent ParsingErrorMessage where
  showErrorComponent _ = error "we don't know what to show for error components"

lexer :: (SemanticErrorEff :> es) => FilePath -> String -> (Eff es) [[Spanned Token]]
lexer filePath text = do
  case lex filePath text of
      Left err -> do
        semPushDiagnostic err
        semCommit
      Right x -> pure x
      
lex :: String -> String -> Either (Diag.Diagnostic String) [[Spanned Token]]
lex filePath fileText =
  case parse megalexer filePath fileText of
    Left bundle -> do
      let diag = Diag.errorDiagnosticFromBundle Nothing "File Failed To Lex" Nothing bundle
      let added = Diag.addFile diag filePath fileText

      Left added
    Right content -> do
      Right $ filter (not . null) $ catMaybes <$> (fmap . fmap) (traverse asNonTrivia) content

megalexer :: LParser [[Spanned Token]]
megalexer = (lex_lines `sepBy` newline) <* eof
  where
    lex_lines = many (spannedT keywords <|> spannedT structure <|> spannedT ident <|> spannedT op <|> spannedT (try comment))
    keywords =
      choice
        [ VoidKw <$ string "void",
          ImplicitKw <$ string "implicit",
          ComputedKw <$ string "computed"
        ]
    structure =
      choice
        [ LBrace <$ char '{',
          Comma <$ char ',',
          RBrace <$ char '}',
          LParen <$ char '(',
          RParen <$ char ')',
          LBrac <$ char '[',
          RBrac <$ char ']'
        ]

    comment :: LParser Token
    comment = Comment <$> (try hspace *> char '#' *> manyTill (satisfy $ const True) (lookAhead $ void eol <|> eof))

    ident =
      ( do
          leading <- satisfy isAlpha
          trailing <- many (satisfy (\x -> isAlphaNum x || x == '_'))

          return $ Ident (leading : trailing)
      )
        <?> "chemical name or rate constant"

    op =
      choice
        [ BiOp <$ string "<==>",
          BackwardsOp <$ string "<==",
          ForwardsOp <$ string "==>",
          AddOp <$ string "+",
          EqOp <$ string "=",
          -- the `:` which creates
          -- dimers, polymers, etc
          MerBinding <$ string ":"
        ]

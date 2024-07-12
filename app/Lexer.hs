{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lexer where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints)
import qualified Error.Diagnose.Compat.Megaparsec as Diag
import System.IO (stderr)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

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
  deriving (Show, Eq)

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

spanFromSourcePos :: SourcePos -> SourcePos -> Span
spanFromSourcePos
  (SourcePos _ sline scol)
  (SourcePos _ eline ecol)
    | sline == eline =
        Span (unPos sline) (unPos scol) (unPos ecol)
spanFromSourcePos _ _ = error "multiline span is not supported"

class Spannable a where
  computeSpan :: a -> Span

instance Spannable (Spanned a) where
  computeSpan (Spanned sp _) = sp

data Spanned a = Spanned Span a deriving (Show)

instance Semigroup Span where
  (Span l1 s1 e1) <> (Span l2 s2 e2) | l1 == l2 = Span l1 (s1 `min` s2) (e1 `max` e2)
  _ <> _ = error "ICE: You've encountered a compiler bug. spans are across several lines. this is not allowed"

instance Functor Spanned where
  fmap f (Spanned _span value) = Spanned _span (f value)

instance Foldable Spanned where
  foldMap f (Spanned _ value) = f value

instance Traversable Spanned where
  traverse f (Spanned _span value) = Spanned _span <$> f value

unspanned :: Spanned a -> a
unspanned (Spanned _ v) = v

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

type LParser = Parsec Void String

instance HasHints Void msg where
  hints _ = mempty

lex :: String -> String -> Either (Diag.Diagnostic String) [[Spanned Token]]
lex filePath fileText =
  case parse lexer filePath fileText of
    Left bundle -> do
      let diag = Diag.errorDiagnosticFromBundle Nothing "File Failed To Lex" Nothing bundle
      let added = Diag.addFile diag filePath fileText

      Left added
    Right content -> do
      Right $ filter (not . null) $ catMaybes <$> (fmap . fmap) (traverse asNonTrivia) content

lexer :: LParser [[Spanned Token]]
lexer = (lex_lines `sepBy` newline) <* eof
  where
    lex_lines = many (spannedT keywords <|> spannedT structure <|> spannedT ident <|> spannedT op <|> spannedT (try comment))
    keywords =
      choice
        [ VoidKw <$ string "void"
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
          EqOp <$ string "="
        ]

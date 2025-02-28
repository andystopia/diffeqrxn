module Utils where

data ExportFormat = JuliaExport | TypstExport | AsciiExport deriving (Show)

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

data Span = Span {spanLine :: Int, spanStartCol :: Int, spanEndCol :: Int} deriving (Show)

-- | reduce the span down to just the last column
squashSpanEnd :: Span -> Span
squashSpanEnd inspan =
  Span
    { spanLine = spanLine inspan,
      spanStartCol = spanEndCol inspan,
      spanEndCol = spanEndCol inspan
    }

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

sequenceSpan :: [Spanned a] -> Spanned [a]
sequenceSpan [] = error "cannot compute span on zero elements"
sequenceSpan [Spanned sp v] = Spanned sp [v]
sequenceSpan ((Spanned sp v) : rest) =
  let (Spanned restSpan restEls) = sequenceSpan rest
      fullSpan = sp <> restSpan
   in Spanned fullSpan (v : restEls)

unspanned :: Spanned a -> a
unspanned (Spanned _ v) = v

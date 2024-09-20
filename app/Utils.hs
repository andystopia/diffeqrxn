module Utils where


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

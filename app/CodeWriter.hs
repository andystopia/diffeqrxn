{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module CodeWriter
  ( CodeWriter,
    runStringCodeWriter,
    runStringCodeWriterEff,
    evalStringCodeWriter,
    execStringCodeWriter,
    indent,
    indented,
    scoped,
    newline,
    dedent,
    push,
  )
where

import Data.Bifunctor (second)
import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.State.Static.Local (gets, modify, runState)

-- ----------------------------- --

-- | Abstract Definitions      | --
-- ----------------------------- --

-- | An effect that describes the process
-- | of writing code in most modern language
data CodeWriter :: Effect where
  Indent :: CodeWriter m ()
  Dedent :: CodeWriter m ()
  Indented :: Eff '[CodeWriter] es -> CodeWriter m ()
  Push :: String -> CodeWriter m ()

-- note that this is a dynamically dispatched algebraic effect
type instance DispatchOf CodeWriter = Dynamic

-- ------------------------------------------ --

-- | SENDERS: These are simply boilerplate  | --
-- | because Haskell is not powerful enough | --
-- ------------------------------------------ --
indent :: (CodeWriter :> es) => (Eff es) ()
indent = send Indent

indented :: (CodeWriter :> es1) => Eff '[CodeWriter] es2 -> Eff es1 ()
indented res = send (Indented res)

dedent :: (CodeWriter :> es) => (Eff es) ()
dedent = send Dedent

push :: (CodeWriter :> es) => String -> Eff es ()
push contents = send (Push contents)

-- ------------------------------------------ --

-- | Helper Functions                       | --
-- ------------------------------------------ --


scoped:: (CodeWriter :> es1) => String -> Eff '[CodeWriter] es2 -> Eff es1 ()
scoped line sub = do
  push line
  indented sub


newline :: (CodeWriter :> es) => Eff es ()
newline = push "\n"

-- ------------------------------------------ --

-- | Concrete Implementations               | --
-- ------------------------------------------ --
data StringWriterState = StringWriterState
  { indentSize :: Int,
    indentLevel :: Int,
    codeLines :: [String]
  }

runStringCodeWriterEffL :: Int -> Eff (CodeWriter : es) a -> (Eff es) (a, [String])
runStringCodeWriterEffL codeIndentSize = do
  (fmap . fmap) (second codeLines) $ reinterpret (runState (StringWriterState codeIndentSize 0 [])) $ \_ -> \case
    Indent -> do
      modify (\s -> s {indentLevel = indentLevel s + 1})
      return ()
    Dedent -> do
      modify (\s -> s {indentLevel = indentLevel s - 1})
    Indented subState -> do
      let res = snd $ runPureEff . runStringCodeWriterEffL codeIndentSize $ subState

      is <- gets indentSize
      il <- (+ 1) <$> gets indentLevel

      let lineIndent = replicate (is * il) ' '

      let indented = (lineIndent <>) <$> res
      modify (\s -> s {codeLines = codeLines s ++ indented})
      return ()
    Push str -> do
      is <- gets indentSize
      il <- gets indentLevel

      let lineIndent = replicate (is * il) ' '
      let indentedLine = lineIndent ++ str

      modify (\s -> s {codeLines = codeLines s ++ [indentedLine]})

      return ()

runStringCodeWriterEff :: Int -> Eff (CodeWriter : es) a -> (Eff es) (a, String)
runStringCodeWriterEff codeIndentSize = fmap (second unlines) <$> runStringCodeWriterEffL codeIndentSize

runStringCodeWriter :: Int -> Eff '[CodeWriter] a -> (a, String)
runStringCodeWriter codeIndentLevel = runPureEff . runStringCodeWriterEff codeIndentLevel

evalStringCodeWriter :: Int -> Eff '[CodeWriter] a -> String
evalStringCodeWriter codeIndentLevel = snd . runPureEff . runStringCodeWriterEff codeIndentLevel

execStringCodeWriter :: Int -> Eff '[CodeWriter] a -> a
execStringCodeWriter codeIndentLevel = fst . runPureEff . runStringCodeWriterEff codeIndentLevel

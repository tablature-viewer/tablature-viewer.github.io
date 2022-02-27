module Utils where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Either (Either(..), hush)
import Data.Enum (class Enum)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple, fst, snd)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser(..), try, unParser)
import Text.Parsing.StringParser.Combinators (lookAhead, many, many1, many1Till, manyTill)

-- Show is for debugging, Print has to give a string that is actually how it is supposed to be presented to the user.
class Print a where
  print :: a -> String

class Enum a <= CyclicEnum a where
  succ' :: a -> a
  pred' :: a -> a

foreach :: forall a b s. s -> List a -> (s -> a -> Tuple s b) -> List b
foreach _ Nil _ = Nil
foreach state (x : xs) loop = snd next : (foreach (fst next) xs loop)
  where
  next = loop state x

foreach' :: forall a s. s -> List a -> (s -> a -> s) -> s
foreach' state Nil _ = state
foreach' state (x : xs) loop = foreach' nextState xs loop
  where
  nextState = loop state x

foreachM :: forall m a s. MonadState s m => List a -> (a -> m Unit) -> m Unit
foreachM Nil _ = pure unit
foreachM (x : xs) loop = do
  loop x
  foreachM xs loop

applyUntilIdempotent :: forall a. (Eq a) => (a -> a) -> a -> a
applyUntilIdempotent f x = if result == x then result else applyUntilIdempotent f result
  where
  result = f x

unsafeTestRegex :: String -> String -> Boolean
unsafeTestRegex patternString text = test (unsafePartial $ fromJust $ hush $ regex patternString noFlags) text

-- NOTES
-- many p will get stuck in a loop if p possibly doesn't consume any input but still succeeds
-- many (many p) will get stuck for any p
-- parseEndOfLine doesn't consume input at the end of the file but still succeeds

-- TODO: make pull request for this combinator
-- Fails with parse error if parser did not consume any input
assertConsume :: forall a. Parser a -> Parser a
assertConsume p = Parser $ \posStrBefore ->
  case unParser p posStrBefore of
    Right result ->
      if posStrBefore.pos < result.suffix.pos then Right result
      else Left { pos: result.suffix.pos, error: "Consumed no input." }
    x -> x

safeMany :: forall a. Parser a -> Parser (List a)
safeMany = many <<< assertConsume

safeMany1 :: forall a. Parser a -> Parser (NonEmptyList a)
safeMany1 = many1 <<< assertConsume

safeManyTill :: forall a end. Parser a -> Parser end -> Parser (List a)
safeManyTill p = manyTill (assertConsume p)

safeMany1Till :: forall a end. Parser a -> Parser end -> Parser (NonEmptyList a)
safeMany1Till p = many1Till (assertConsume p)

safeLookAhead :: forall a. Parser a -> Parser a
safeLookAhead = try <<< lookAhead

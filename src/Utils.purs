module Utils where

import Prelude

import Data.List (List(..), (:))
import Data.Enum (class Enum)
import Data.Tuple (Tuple, fst, snd)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (Either(..))
import Text.Parsing.StringParser (Parser(..), unParser)
import Text.Parsing.StringParser.Combinators (many, many1, many1Till, manyTill)

class Enum a <= CyclicEnum a where
  succ' :: a -> a
  pred' :: a -> a

debug :: forall a. String -> a -> a
debug msg = snd $ unsafePerformEffect $ log msg
  where snd _ b = b

foreach :: forall a b s. s -> List a -> (s -> a -> Tuple s b) -> List b
foreach _ Nil _ = Nil
foreach state (x : xs) loop = snd next : (foreach (fst next) xs loop)
  where next = loop state x

applyUntilIdempotent :: forall a. (Eq a) => (a -> a) -> a -> a
applyUntilIdempotent f x = if result == x then result else applyUntilIdempotent f result
  where result = f x

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
      if posStrBefore.pos < result.suffix.pos
      then Right result
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

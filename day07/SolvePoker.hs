module Solve (main) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, second)
import Data.Char (isDigit)
import Data.List (group, sort, sortBy, sortOn)
import Data.Ord (Down (Down))

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
  deriving (Eq, Ord, Show)

-- Pretend that the constructors are in another module and not exported :^)
newtype UT2 a = UT2 (a, a)
  deriving (Eq, Ord, Show)

newtype UT3 a = UT3 (a, a, a)
  deriving (Eq, Ord, Show)

newtype UT5 a = UT5 (a, a, a, a, a)
  deriving (Eq, Ord, Show)

mkUT2 :: (Ord a) => a -> a -> UT2 a
mkUT2 a b = UT2 (max a b, min a b)

mkUT3 :: (Ord a) => a -> a -> a -> UT3 a
mkUT3 a b c | [a, b, c] <- sortOn Down [a, b, c] = UT3 (a, b, c)

mkUT5 :: (Ord a) => a -> a -> a -> a -> a -> UT5 a
mkUT5 a b c d f | [a, b, c, d, f] <- sortOn Down [a, b, c, d, f] = UT5 (a, b, c, d, f)

data Hand
  = HighCard (UT5 Card)
  | OnePair Card (UT3 Card)
  | TwoPair (UT2 Card) Card
  | ThreeOfAKind Card (UT2 Card)
  | FullHouse Card Card
  | FourOfAKind Card Card
  | FiveOfAKind Card
  deriving (Eq, Ord, Show)

parseCard :: Char -> Card
parseCard 'A' = A
parseCard 'K' = K
parseCard 'Q' = Q
parseCard 'J' = J
parseCard 'T' = T
parseCard x | x `elem` ['2' .. '9'] = [N2, N3, N4, N5, N6, N7, N8, N9] !! (read [x] - 2)
parseCard _ = error "Invalid card"

counts :: (Ord a) => [a] -> [(a, Int)]
counts = sortBy sorting . fmap (head &&& length) . group . sort
  where
    sorting = curry (uncurry (flip compare) . bimap snd snd)

parseHand :: String -> Hand
parseHand hand = case counts $ fmap parseCard hand of
  [(a, 5)]                                 -> FiveOfAKind a
  [(a, 4), (b, 1)]                         -> FourOfAKind a b
  [(a, 3), (b, 2)]                         -> FullHouse a b
  [(a, 3), (b, 1), (c, 1)]                 -> ThreeOfAKind a (mkUT2 b c)
  [(a, 2), (b, 2), (c, 1)]                 -> TwoPair (mkUT2 a b) c
  [(a, 2), (b, 1), (c, 1), (d, 1)]         -> OnePair a (mkUT3 b c d)
  [(a, 1), (b, 1), (c, 1), (d, 1), (e, 1)] -> HighCard $ mkUT5 a b c d e

parseInput :: String -> [(Hand, Int)]
parseInput = fmap (parseBet . words) . lines
  where parseBet [hand, bet] = (parseHand hand, read bet)

part1 :: [(Hand, Int)] -> Int
part1 = sum . fmap (uncurry (*) . second snd) . zip [1 ..] . sortOn fst

main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  pure ()

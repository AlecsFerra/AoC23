module Solve (main) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, second)
import Data.Char (isDigit)
import Data.List (group, sort, sortBy, sortOn)
import Data.Ord (Down (Down))
import Debug.Trace

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
  deriving (Eq, Ord, Show)

newtype Card2 = Card2 Card
  deriving Eq

instance Ord Card2 where
  (Card2 J) <= _         = True
  (Card2 a) <= (Card2 b) = a <= b

data Hand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

parseCard 'A' = A
parseCard 'K' = K
parseCard 'Q' = Q
parseCard 'J' = J
parseCard 'T' = T
parseCard x | x `elem` ['2' .. '9'] = [N2, N3, N4, N5, N6, N7, N8, N9] !! (read [x] - 2)

counts = sortBy (curry (uncurry (flip compare) . bimap snd snd)) 
       . fmap (head &&& length) 
       . group 
       . sort

parseHand1 hand = case counts cards of
    [(a, 5)]                                 -> (FiveOfAKind, cards)
    [(a, 4), (b, 1)]                         -> (FourOfAKind, cards)
    [(a, 3), (b, 2)]                         -> (FullHouse, cards)
    [(a, 3), (b, 1), (c, 1)]                 -> (ThreeOfAKind, cards)
    [(a, 2), (b, 2), (c, 1)]                 -> (TwoPair, cards)
    [(a, 2), (b, 1), (c, 1), (d, 1)]         -> (OnePair, cards)
    [(a, 1), (b, 1), (c, 1), (d, 1), (e, 1)] -> (HighCard, cards)
  where cards = fmap parseCard hand

parseHand2 hand = case counts cards of
    [(a, 5)]                                 -> (FiveOfAKind, cards)

    [(a, 4), (b, 1)]                         | J `elem` [a, b] 
                                             -> (FiveOfAKind, cards)
    [(a, 4), (b, 1)]                         -> (FourOfAKind, cards)

    [(a, 3), (b, 2)]                         | J `elem` [a, b] 
                                             -> (FiveOfAKind, cards)
    [(a, 3), (b, 2)]                         -> (FullHouse, cards)

    [(a, 3), (b, 1), (c, 1)]                 | J `elem` [a, b, c]
                                             -> (FourOfAKind, cards)
    [(a, 3), (b, 1), (c, 1)]                 -> (ThreeOfAKind, cards)

    [(a, 2), (b, 2), (c, 1)]                 | J `elem` [a, b] 
                                             -> (FourOfAKind, cards)
    [(a, 2), (b, 2), (c, 1)]                 | J == c 
                                             -> (FullHouse, cards)
    [(a, 2), (b, 2), (c, 1)]                 -> (TwoPair, cards)

    [(a, 2), (b, 1), (c, 1), (d, 1)]         | J `elem` [a, b, c, d]
                                             -> (ThreeOfAKind, cards)
    [(a, 2), (b, 1), (c, 1), (d, 1)]         -> (OnePair, cards)

    [(a, 1), (b, 1), (c, 1), (d, 1), (e, 1)] | J `elem` [a, b, c, d, e]
                                             -> (TwoPair, cards)
    [(a, 1), (b, 1), (c, 1), (d, 1), (e, 1)] -> (HighCard, cards)
  where cards = fmap parseCard hand

solve handParsing handOrd = sum 
                          . fmap (uncurry (*) . second snd) 
                          . zip [1 ..] 
                          . sortOn (handOrd . fst) 
                          . fmap (parseBet . words) . lines
      where parseBet [hand, bet] = (handParsing hand, read bet)

part1 = solve parseHand1 id
part2 = solve parseHand2 $ second $ fmap Card2

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

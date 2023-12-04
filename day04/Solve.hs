module Solve (main) where

import Data.Bifunctor (bimap, first)
import Data.List (elemIndex, splitAt)
import Data.Set (Set)
import Data.Set qualified as S (fromList, intersection, size)
import Control.Arrow ((&&&))

splitOn s x
  | Just i <- elemIndex s x = splitAt i x
  | otherwise = (x, [])

parseGame = bimap (parse . snd . splitOn ':') parse
          . splitOn '|'
  where parse = fmap (read @Int)
              . words
              . tail

wins = fmap $ S.size 
            . uncurry S.intersection 
            . bimap S.fromList S.fromList 
            . parseGame

part1 = sum 
      . fmap points 
      . wins
  where points 0 = 0
        points x = 2 ^ (x - 1)

part2 :: [[Char]] -> Int
part2 = sum . foldr acc [] . wins
  where 
    acc = curry $ uncurry (:) . first ((+1) . sum . uncurry take) . (id &&& snd)

main = do
  input <- lines <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

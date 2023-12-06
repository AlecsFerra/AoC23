module Solve (main) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)

parse = ((!! 0) &&& (!! 1))
      . fmap (tail . words)
      . lines

part1 = product
      . fmap (uncurry solve . bimap read read)
      . uncurry zip

part2 = uncurry solve
      . bimap num num
  where num = read . concat

solve time distance | even time = delta2       `div` 2 * 2 + 1
                    | otherwise = (delta2 + 1) `div` 2 * 2
  where
    delta2  = floor $ sqrt (fromIntegral $ time ^ 2 - 4 * distance)

main = do
  input <-  parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

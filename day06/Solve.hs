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

solve time distance = search ok         max time
                    - search (not . ok) 0   max
  where
    mid x y = x + (y - x) `div` 2
    max = time `div` 2
    ok hold = (time - hold) * hold > distance

    -- equiv max $ filter p [l .. h]
    search p l h | l + 1 == h  = l
                 | p mid       = search p mid h
                 | otherwise   = search p l mid
      where mid = l + (h - l) `div` 2

main = do
  input <-  parse <$> readFile "input.txt" :: IO ([String], [String])
  print $ part1 input
  print $ part2 input
  pure ()

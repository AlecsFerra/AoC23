module Solve (main) where

import Data.List (foldl')
import Control.Arrow ((&&&))
import Control.Monad (ap)

parse = fmap (fmap (read @Int) . words) . lines

part1 = sum . fmap (foldl' (+) 0 . fmap last . sequences)
  where sequences = takeWhile (not . all (== 0))
                  . iterate step
        step = fmap (uncurry (flip (-)))
             . ap zip tail

part2 = part1 . fmap reverse

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

module Solve (main) where

import Control.Monad (ap)

parse = fmap (fmap (read @Int) . words) . lines

part1 = sum . fmap (sum . fmap last . sequences)
  where sequences = takeWhile (any (/= 0))
                  . iterate step
        step = fmap (uncurry subtract)
             . ap zip tail

part2 = part1 . fmap reverse

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

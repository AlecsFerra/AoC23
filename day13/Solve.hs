module Solve (main) where

import Control.Arrow ((&&&), (***), second)
import Data.List (inits, tails, transpose)
import Data.List.Split (splitOn)
import Control.Monad (guard)

(.:) = (.) . (.)
infixr 7 .:

parse = fmap lines . splitOn "\n\n"

reflectAt target map = do
  (index, upper, lower) <- zip3 [0..] (inits map) (tails map)
  guard $ not . null $ upper
  guard $ not . null $ lower

  let diff = countDifferences (reverse upper) lower
  guard $ diff == target

  pure index

  where countDifferences = sum .: zipWith (length . filter not .: zipWith (==))

solve target = uncurry (+) 
             . (sum . reflectAt target . transpose 
                &&& 
                (* 100) . sum . reflectAt target)

part1 = sum . fmap (solve 0)
part2 = sum . fmap (solve 1)

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()



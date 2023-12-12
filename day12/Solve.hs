module Solve (main) where

import Data.List.Split (splitOn)
import Data.List (elemIndex, intercalate)
import Control.Arrow ((&&&), second, (***))
import Data.Function.Memoize (memoize2)
import Data.Maybe (fromJust)
import Data.Tuple (swap)


parse = fmap parse' . lines
  where parse' = second (fmap (read @Int) . splitOn ",")
               . uncurry splitAt
               . (fromJust . elemIndex ' ' &&& id)

solution pattern springs = solve 0 0
  where
        solve = memoize2 solve'

        solve' :: Int -> Int -> Int
        solve' p s | p >= length pattern
                   , '#' `notElem` drop s springs
                   = 1
        solve' p s | p >= length pattern
                   = 0
        solve' p s | s >= length springs
                   = 0
        solve' p s | '.' == springs !! s
                   = solve p $ s + 1
        solve' p s | '#' == springs !! s
                   , len <- pattern !! p
                   , s + len <= length springs
                   , '.' `notElem` take len ( drop s springs)
                   , s + len == length springs || springs !! (s + len) /= '#'
                   = solve (p + 1) (s + len + 1)
        solve' p s | '#' == springs !! s
                   = 0
        solve' p s | '?' == springs !! s
                   , len <- pattern !! p
                   , s + len <= length springs
                   , '.' `notElem` take len (drop s springs)
                   , s + len == length springs || springs !! (s + len) /= '#'
                   = solve (p + 1) (s + len + 1) + solve p (s + 1)
        solve' p s | '?' == springs !! s
                   = solve p (s + 1)

part1 = sum . fmap (uncurry solution . swap)

part2 = sum . fmap (uncurry solution
                   . swap
                   . (intercalate "?" . replicate 5 *** concat . replicate 5))

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

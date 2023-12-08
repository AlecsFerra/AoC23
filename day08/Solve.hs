module Solve (main) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List (findIndex, foldl')
import Data.Map as M (fromList, lookup, keys)
import Data.Maybe (fromJust)

parse = (head &&& fmap entry . drop 2) . lines
  where entry = take 3 &&& (take 3 &&& take 3 . drop 5) . drop 7

solve from to = follow . bimap cycle M.fromList
  where follow (steps, mapping) = fromJust . findIndex to $ scanl merge from steps
          where merge acc 'L' = fst . fromJust $ M.lookup acc mapping
                merge acc 'R' = snd . fromJust $ M.lookup acc mapping

part1 = solve "AAA" (== "ZZZ")

part2 s@(_, m) = foldl' lcm 1 
               $ (\a -> solve a ((== 'Z') . last) s)
              <$> filter ((== 'A') . last) (fmap fst m)

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

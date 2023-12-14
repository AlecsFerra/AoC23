module Solve (main) where

import Data.List (transpose, intercalate, foldl')
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import Data.Function (fix)
import qualified Data.Map as M (Map, empty, lookup, insert)

west = move id
north = transpose . west . transpose
east = move reverse
south = transpose . east . transpose

move i = fmap $ i
              . intercalate "#" 
              . fmap (uncurry (++) . (shift 'O' &&& shift '.')) 
              . splitOn "#"
              . i
  where shift s = flip replicate s . length . filter (== s)

load = sum 
     . map (sum . map fst . filter ((== 'O') . snd) . zip [1..] . reverse) 
     . transpose

part1 = load . north

part2 = load
      . snd
      . uncurry (!!)
      . (id &&& uncurry actual . idxs M.empty)
      . zip [0 ..] 
      . iterate (foldl' (.) id [east, south, west, north])
  where 
      idxs m ((i, x):xs) | Just j <- M.lookup x m = (i, j)
                         | otherwise              = idxs (M.insert x i m) xs

      actual s e = s + (1_000_000_000 - s) `rem` (e - s)

main = do
  input <- lines <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

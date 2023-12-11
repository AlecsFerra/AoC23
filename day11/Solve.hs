{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move concatMap out" #-}

module Solve (main) where

import Control.Arrow (first, (&&&))
import Data.Set as S (fromList, difference, size, intersection)
import Data.List (tails)
import Control.Monad (guard)

parse = galaxyPoistion
      . concatMap (uncurry $ fmap . first . (,))
      . zip [0 ..]
      . fmap (zip [0 ..])
      . lines
      where
        galaxyPoistion = fmap fst
                       . filter ((== '#') . snd)

empty selector = uncurry S.difference
               . ((S.fromList . uncurry enumFromTo . (minimum &&& maximum)) &&& S.fromList)
               . fmap selector

pairs xs = [ (x, y) | x:s <- tails xs , y <- s ]

intersections points x1 x2 = S.size 
                           $ S.intersection points 
                           $ S.fromList [min x1 x2 .. max x1 x2]

solve size galaxies rows cols = sum $ distance <$> pairs galaxies
  where distance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)
                                      + size * intersections rows x1 x2
                                      + size * intersections cols y1 y2

part1 = solve 1
part2 = solve $ 1_000_000 - 1

main = do
  galaxies <- parse <$> readFile "input.txt"
  let expandingRows = empty fst galaxies
  let expandingCols = empty snd galaxies
  print $ part1 galaxies expandingRows expandingCols
  print $ part2 galaxies expandingRows expandingCols


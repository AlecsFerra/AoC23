module Solve (main) where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Bifunctor (bimap, first, second)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Tuple (swap)

parseFile src
  | header : maps <- splitOn "\n\n" src =
      (fmap (read @Int) $ tail $ words header, fmap parseRange . tail . lines <$> maps)
  where
    parseRange line
      | [dst, src, range] <- read @Int <$> words line =
          ((src, src + range), dst - src)

solve maps seeds = fst $ minimum $ foldl' applyAllRules seeds maps

applyAllRules seeds rules = uncurry (++) $ foldl' acc (seeds, []) rules

acc (toApply, applied) map = second (applied ++) $ swap $ applyAllSeeds toApply map

applyAllSeeds seeds map = bimap catMaybes join $ unzip $ fmap (`applySeed` map) seeds

applySeed seed (range, val) = first (fmap $ rangeAdd val) $ intersect seed range

rangeAdd val = bimap (+ val) (+ val)

intersect (x, y) (l, u)
  | x > u = (Nothing, [(x, y)])
  | y < l = (Nothing, [(x, y)])
  | x >= l && y <= u = (Just (x, y), [])
  | y <= u = (Just (l, y), [(x, l - 1)])
  | x >= l = (Just (x, u), [(u + 1, y)])
  | otherwise = (Just (l, u), [(x, l - 1), (u + 1, y)])

pairs (x : y : rest) = (x, x + y - 1) : pairs rest
pairs [] = []

main = do
  input <- readFile "input.txt"
  let (seeds, maps) = parseFile input
  print $ solve maps $ fmap (id &&& id) seeds
  print $ solve maps $ pairs seeds
  pure ()

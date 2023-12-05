module Solve (main) where

import Control.Arrow ((&&&))
import Control.Monad (ap, join)
import Data.Bifunctor (bimap, first, second)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Tuple (swap)

(.:) = (.) . (.)

infixr 8 .:

parseFile src
  | header : maps <- splitOn "\n\n" src =
      (fmap (read @Int) $ tail $ words header, fmap parseRange . tail . lines <$> maps)
  where
    parseRange line
      | [dst, src, range] <- read @Int <$> words line =
          ((src, src + range), dst - src)

solve = fst . minimum .: foldl' applyAllRules

applyAllRules = curry $ uncurry (++) . uncurry (foldl' acc) . first (,[])

acc = uncurry (flip ((.) . second . (++)) . (swap .) . flip applyAllSeeds)

applyAllSeeds = curry $ bimap catMaybes join . unzip . uncurry fmap . first (flip applySeed)

applySeed = (`ap` snd) . (. fst) . (flip (first . fmap . rangeAdd) .) . intersect

rangeAdd = uncurry bimap . ((+) &&& (+))

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
  print . flip solve maps $ fmap (id &&& id) seeds
  print . flip solve maps $ pairs seeds
  pure ()

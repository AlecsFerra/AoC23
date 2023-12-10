{-# HLINT ignore "Move concatMap out" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE NondecreasingIndentation #-}

module Solve (main) where

import Control.Applicative (asum)
import Control.Arrow (first, second, (&&&))
import Control.Monad (guard, liftM2, filterM, liftM)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (runStateT, gets, modify, lift)
import Data.Map (Map)
import Data.Map as M (fromList, lookup)
import Data.Set (Set)
import Data.Set as S (empty, insert, member)
import Data.Maybe (fromJust)

parse = (starting &&& flatten)
    . filter ((/= '.') . snd)
    . concatMap (uncurry $ fmap . first . (,))
    . zip [0 ..]
    . fmap (zip [0 ..])
    . lines
  where
    starting = fst . head . filter ((== 'S') . snd)
    flatten  = M.fromList . fmap (uncurry $ liftM2 (.) (,) (flip tubes))

    tubes '|' = flip fmap [south, north]             . flip ($)
    tubes '-' = flip fmap [west,  east]              . flip ($)
    tubes 'L' = flip fmap [north, east]              . flip ($)
    tubes 'J' = flip fmap [north, west]              . flip ($)
    tubes '7' = flip fmap [west,  south]             . flip ($)
    tubes 'F' = flip fmap [east,  south]             . flip ($)
    tubes 'S' = flip fmap [north, south, east, west] . flip ($)

    north = first  (subtract 1)
    south = first  (+        1)
    east  = second (+ 1)
    west  = second (subtract 1)


loopFrom g s = fst
            <$> runStateT (runReaderT (go s) g) S.empty
  where -- Transformer operations
        mark = modify . S.insert
        visited = gets . S.member
        neighbors p = ask >>= lift . lift . M.lookup p

        go p = do
          alreadyVisited <- visited p
          if p == s && alreadyVisited then pure [p] else do -- Early return if we 
                                                            -- reached the end
          guard $ not alreadyVisited                        -- Skip double visits

          mark p                                            -- Mark as visited
          n <- neighbors p                                  -- Get all neighbors
          k <- asum $ fmap go n                             -- First neighbors that
                                                            -- has a path to the end
          pure $ p : k

corners a = head a : fmap snd3 (filter (\(a, b, c) -> corner a c) $ zip3 a (tail a) (tail $ tail a))
  where corner (a1, b1) (a2, b2) = a1 /= a2 && b1 /= b2
        snd3 (_, x, _) = x

area x = abs (t xs ys - t ys xs) `div` 2
  where
    xs = fmap fst x
    ys = fmap snd x
    t x y = sum $ (last x * head y) : zipWith (*) x (tail y)

part1 = (`div` 2) . length

part2 = area . corners

main = do
  input <- parse <$> readFile "input.txt"

  let loop = fromJust $ uncurry (flip loopFrom) input
  let p1 = part1 loop

  print p1
  print $ part2 loop - p1 + 1
  pure ()

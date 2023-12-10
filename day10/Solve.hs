{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move concatMap out" #-}
{-# LANGUAGE BlockArguments #-}

module Solve (main) where

import Data.Map (Map, lookup)
import Data.Map as M (fromList, lookup)
import Data.Set (Set)
import Data.Set as S (insert, member, empty, singleton)
import Control.Arrow (first, second, (&&&))
import Control.Monad (liftM2, join)
import Data.Maybe (mapMaybe, fromJust)

type Position = (Int, Int)

parse = (
        fst . head . filter ((== 'S') . snd)
        &&&
        M.fromList . fmap (uncurry $ liftM2 (.) (,) (flip tubes))
      )
      . filter ((/= '.') . snd)
      . concatMap (uncurry $ fmap . first . (,))
      . zip [0 ..]
      . fmap (zip [0 ..])
      . lines
      where tubes '|' = flip fmap [ first  (+ 1)
                                  , first  (subtract 1)
                                  ] . flip ($)
            tubes '-' = flip fmap [ second (+ 1)
                                  , second (subtract 1)
                                  ] . flip ($)
            tubes 'L' = flip fmap [ first  (subtract 1)
                                  , second (+ 1)
                                  ] . flip ($)
            tubes 'J' = flip fmap [ first  (subtract 1)
                                  , second (subtract 1)
                                  ] . flip ($)
            tubes '7' = flip fmap [ second (subtract 1)
                                  , first  (+ 1)
                                  ] . flip ($)
            tubes 'F' = flip fmap [ first  (+ 1)
                                  , second (+ 1)
                                  ] . flip ($)
            tubes 'S' = flip fmap [ second (subtract 1)
                                  , second (+ 1)
                                  , first  (+ 1)
                                  , first  (subtract 1)
                                  ] . flip ($)

safeHead [] = Nothing
safeHead x  = Just $ head x

loop start graph = head
                 $ mapMaybe (go start graph $ S.singleton start)
                 $ fromJust 
                 $ M.lookup start graph
  where
    go start graph visited from | start == from         = Just [from]
                                | S.member from visited = Nothing
                                | otherwise             = (fmap (from : ) . safeHead . mapMaybe (go start graph (S.insert from visited))) =<< M.lookup from graph

part1 x = length (uncurry loop x) `div` 2

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  pure ()

module Solve (main) where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Control.Arrow (first, second, (***))
import Control.Monad.State (evalState, modify, gets, put, runState)
import Control.Monad.Trans (lift)
import Control.Monad (forM_, forever, when)
import Data.Map as M (fromList, empty, insert, lookup, keys, member)
import Data.PSQueue as PQ (empty, adjust, findMin, Binding ((:->)), deleteMin, fromAscList)
import Debug.Trace

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addD :: (Num a) => Distance a -> Distance a -> Distance a
addD (Dist x) (Dist y) = Dist (x + y)
addD _ _ = Infinity

parse = M.fromList
      . concatMap (uncurry $ fmap . first . (,))
      . zip [0 ..]
      . fmap (zip [0 ..] . fmap (Dist . digitToInt))
      . lines

type P = (Int, Int)
type D = Distance Int

whileJust_ k z = do
  m <- k
  case m of
    Nothing -> pure ()
    Just m  -> do
      z m
      whileJust_ k z

dijkstra map from to = runState go (PQ.empty, M.empty)
  where
        getMin = do
          min <- gets $ PQ.findMin . fst
          modify $ first PQ.deleteMin
          pure min
        
        adjustDistance u d p = modify (PQ.adjust (const d) u *** M.insert u (d, p))
        getDistance d        = gets $ fst . fromJust . M.lookup d . snd
        getParent   d        = gets $ snd . fromJust . M.lookup d . snd
       
        streak u s = do
          p <- getParent u
          case p of
            Nothing -> pure 0
            Just p' -> if s u p' then do
              r <- streak p' s
              pure $ r + 1
            else 
              pure 0

        go = do
          -- Initialization
          put (PQ.fromAscList [ k PQ.:-> Infinity        | k <- M.keys map ], 
               M.fromList     [ (k, (Infinity, Nothing)) | k <- M.keys map])

          adjustDistance from (Dist 0) Nothing

          whileJust_ getMin $ \(u@(ux, uy) PQ.:-> du) -> do
              ml <- streak (traceShowId u) $ \(x, y) (xp, yp) -> x == xp + 1
              mr <- streak u $ \(x, y) (xp, yp) -> x == xp - 1
              md <- streak u $ \(x, y) (xp, yp) -> y == yp + 1
              mu <- streak u $ \(x, y) (xp, yp) -> y == yp - 1
              
              let neighbors = filter (`M.member` map)
                            $ filter (/= u)
                            $  [(ux + dx, uy) | dx <- [0 .. min 1 (3 - ml)]]
                            ++ [(ux - dx, uy) | dx <- [0 .. min 1 (3 - mr)]]
                            ++ [(ux, uy + dy) | dy <- [0 .. min 1 (3 - md)]]
                            ++ [(ux, uy - dy) | dy <- [0 .. min 1 (3 - mu)]]

              forM_ (traceShowId neighbors) $ \v -> do
                let alt = du `addD` fromJust (M.lookup v map)
                dv <- getDistance v
                when (traceShowId alt < dv) $ do
                  adjustDistance v alt $ Just u

          getDistance to

part1 = dijkstra

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input (0, 0) (12, 12)
  pure ()

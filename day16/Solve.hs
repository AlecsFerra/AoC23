module Solve (main) where

import Control.Arrow (first, (***), (&&&))
import Data.Set as S (fromList, member, insert, empty, map, size)
import Control.Monad (forM_, guard)
import Control.Monad.State (runState, modify, gets)

penta f g h i j x = (f x, g x, h x, i x, j x)

parse = penta (takeCoords '/')
              (takeCoords '\\')
              (takeCoords '-')
              (takeCoords '|')
              (maximum . fmap (fst . fst))
      . concatMap (uncurry $ fmap . first . (,))
      . zip [0 ..]
      . fmap (zip [0 ..])
      . lines
      where
        takeCoords x = S.fromList . fmap fst . filter ((== x) . snd)

data Direction
  = L | R | U | D
  deriving (Eq, Ord)

move U ((x, y), _) = ((x - 1, y), U)
move L ((x, y), _) = ((x, y - 1), L)
move R ((x, y), _) = ((x, y + 1), R)
move D ((x, y), _) = ((x + 1, y), D)

solve (ruMirror, luMirror, hSplitter, vSplitter, bounds) = S.size
                                                         . S.map fst
                                                         . snd
                                                         . flip runState S.empty
                                                         . go
  where go p = do
          visited <- gets $ S.member p
          if not (inBounds p) || visited then 
            pure () 
          else do
            modify $ S.insert p

            let next = applyOne p
            forM_ next go
        
        inBounds = uncurry (&&) . (inBound *** inBound) . fst
        inBound x = x >= 0 && x <= bounds

        applyOne b@(p, d) -- RU MIRRORS /
                          | p `S.member` ruMirror, R <- d
                          = [ move U b ]
                          | p `S.member` ruMirror, L <- d
                          = [ move D b ]
                          | p `S.member` ruMirror, U <- d
                          = [ move R b ]
                          | p `S.member` ruMirror, D <- d
                          = [ move L b ]
                          -- LU MIRRORS \
                          | p `S.member` luMirror, R <- d
                          = [ move D b ]
                          | p `S.member` luMirror, L <- d
                          = [ move U b ]
                          | p `S.member` luMirror, U <- d
                          = [ move L b ]
                          | p `S.member` luMirror, D <- d
                          = [ move R b ]
                          -- H splitter -
                          | p `S.member` hSplitter
                          = [ move R b, move L b ]
                          -- V splitter |
                          | p `S.member` vSplitter
                          = [ move U b, move D b ]
                          | otherwise
                          = [ move d b ]


part1 r = solve r ((0, 0), R)

part2 r@(_, _, _, _, b) = maximum $ fmap (solve r) directions
  where directions =  [((0, x), D) | x <- [0 .. b]]
                   ++ [((x, 0), R) | x <- [0 .. b]]
                   ++ [((b, x), U) | x <- [0 .. b]]
                   ++ [((x, b), L) | x <- [0 .. b]]

main = do
  f <- parse <$> readFile "input.txt"
  print $ part1 f
  print $ part2 f
  pure ()

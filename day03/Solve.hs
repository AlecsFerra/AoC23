module Solve (main) where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as S

parse =
  concatMap (\(a, b) -> fmap (first (a,)) b)
    . enum
    . map enum
    . lines
  where
    enum = zip [0 ..]

symbol = S.fromList . map fst . filter (isSymbol . snd)
  where isSymbol = uncurry (&&) . (not . isDigit &&& (/= '.'))

gear = map fst . filter ((== '*') . snd)

partNumber [] = []
partNumber x = case (unzip . takeWhile number) x of
  ([], []) -> partNumber $ tail x
  (pos, num) -> (pos, read num) : partNumber (drop (length num) x)
  where
    number = uncurry (&&)
           . (isDigit . snd &&& (==) (fst . fst . head $ x) . fst . fst)

neighbors :: [(Int, Int)] -> Set (Int, Int)
neighbors = foldl' (curry $ uncurry S.union . second around) S.empty
  where
    around = S.fromList
           . (<$> [ second (+ 1)
                  , second (subtract 1)
                  , first (+ 1)
                  , first (subtract 1)
                  , bimap (+ 1) (+ 1)
                  , bimap (+ 1) (subtract 1)
                  , bimap (subtract 1) (subtract 1)
                  , bimap (subtract 1) (+ 1)
                  ])
           . flip ($)

part1 symbols = sum
              . map snd
              . filter (not . S.disjoint symbols . neighbors . fst)

part2 gear parts = sum $ map geared gear
  where product2 [a, b] = a * b
        product2 _ = 0
        geared g = product2 $ map snd $ filter (S.member g . neighbors . fst) parts

main = do
  input <- parse <$> readFile "input.txt"
  let partNumbers = partNumber input
  let symbols = symbol input
  let gears = gear input
  print $ part1 symbols partNumbers
  print $ part2 gears partNumbers
  pure ()

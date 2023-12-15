module Solve (main) where

import Control.Arrow (second)
import Control.Monad (mapM_)
import Control.Monad.ST (runST)
import Data.Char (ord)
import Data.List (foldl', findIndex, splitAt)
import Data.List.Split (splitOn)
import Data.Vector.Mutable as V (modify, replicate)
import Data.Vector as V (freeze, toList)

parse = splitOn "," . head . lines

-- >>> hash "HASH"
-- 52
hash = foldl' (curry hashStep) 0
  where hashStep = (`rem` 256) . (* 17) . uncurry (+) . second ord

part1 = sum . fmap hash

part2 ops = focusingPower
          . V.toList
          $ runST $ do
            v <- V.replicate 256 []
            mapM_ (step v) ops
            V.freeze v

step buckets operation | [lbl, at] <- splitOn "=" operation
                       = V.modify buckets (update (lbl, read at)) (hash lbl)
step buckets operation | [lbl, _]  <- splitOn "-" operation
                       = V.modify buckets (filter $ (/= lbl) . fst) (hash lbl)

update (lbl, val) xs | Nothing <- findIndex ((== lbl) . fst) xs
                     = (lbl, val) : xs
update (lbl, val) xs | Just i  <- findIndex ((== lbl) . fst) xs
                     , (before, after) <- splitAt i xs
                     = before ++ [(lbl, val)] ++ tail after

focusingPower = sum 
              . fmap internal 
              . zip [1..] 
              . fmap (zip [1..] . fmap snd . reverse)
  where internal = uncurry ((sum .) . (. fmap (uncurry (*))) . (<$>) . (*))


main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
  pure ()

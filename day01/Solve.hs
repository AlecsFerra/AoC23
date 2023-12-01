module Solve where

import Data.Char (isDigit, digitToInt)
import Data.List (foldl', isPrefixOf, isSuffixOf)
import Control.Arrow (Arrow((&&&)))

solution first second = foldl' (+) 0 
                      . fmap (uncurry (+) . ((10 *) . first &&& second))

part1 = solution (parse head) (parse last)
  where parse p = digitToInt . p . filter isDigit

part2 = solution (parse isPrefixOf head tail) (parse isSuffixOf last init) 
  where parse c h r s | isDigit $ h s = digitToInt $ h s
                      | "one"   `c` s = 1
                      | "two"   `c` s = 2
                      | "three" `c` s = 3
                      | "four"  `c` s = 4
                      | "five"  `c` s = 5
                      | "six"   `c` s = 6
                      | "seven" `c` s = 7
                      | "eight" `c` s = 8
                      | "nine"  `c` s = 9
                      | otherwise     = parse c h r $ r s

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input

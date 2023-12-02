module Solve where

import Control.Arrow (Arrow (second))
import Data.Either (fromRight)
import Data.List (foldl')
import Text.Parsec (char, digit, many, parse, sepBy1, string, try, (<|>))
import Prelude hiding (round)

-- Trifunctor . Join
trimapJoin f (a, b, c) = (f a, f b, f c)

parseInput = parse (many $ game <* char '\n') ""
  where
    number = read <$> many digit
    round = trimapJoin sum . unzip3 <$> draw `sepBy1` char ','
    color c = string " " *> number <* string " " <* string c

    draw =
      try ((0,0,) <$> color "blue")
        <|> try ((0,,0) <$> color "green")
        <|> try ((,0,0) <$> color "red")

    game = do
      string "Game "
      id <- number
      string ":"
      rounds <- round `sepBy1` char ';'
      pure (id, rounds)

part1 = sum . map fst . filter (all isValidRound . snd)
  where
    isValidRound (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 = sum . fmap (mul . trimapJoin maximum . unzip3 . snd)
  where
    mul (a, b, c) = a * b * c

main = do
  input <- fromRight [] . parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input

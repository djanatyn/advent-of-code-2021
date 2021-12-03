{-# LANGUAGE LambdaCase #-}

module Day3 where

import Data.List (intersect)
import Control.Exception (catch)
import System.Exit (ExitCode(ExitSuccess))
import Numeric (readBin)
import Test.Tasty
import Test.Tasty.HUnit

type Record = [Char]

update :: [Record] -> Record -> [Record]
update accum new = zipWith (:) new accum

mostCommon :: String -> Char
mostCommon string
  | zeroes > ones = '0'
  | ones > zeroes = '1'
  | otherwise = error "equal number of zeroes and ones"
  where
    zeroes = length $ intersect string "0"
    ones = length $ intersect string "1"

opposite :: Char -> Char
opposite '0' = '1'
opposite '1' = '0'
opposite _ = error "bad input"

solve :: [Record] -> Int
solve input =
  let cols = foldl update (replicate (length input) []) input
      gamma = fst . head . readBin $ map mostCommon cols
      epsilon = fst . head . readBin $ map (opposite . mostCommon) cols
   in gamma * epsilon

testExample :: Assertion
testExample = do
  example <- lines <$> readFile "../input/day3-example.txt"
  solve example @?= 198

main :: IO ()
main = catch
  (defaultMain $ testCase "example" testExample)
  (\case
    ExitSuccess -> lines <$> getContents >>= print . solve
    _ -> return ())
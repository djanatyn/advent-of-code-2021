{-# LANGUAGE LambdaCase #-}

module Day03 where

import Control.Exception (catch)
import Data.List (intersect)
import Numeric (readBin)
import System.Exit (ExitCode (ExitSuccess))
import Test.Tasty
import Test.Tasty.HUnit

type Record = [Char]

update :: [Record] -> Record -> [Record]
update accum new = zipWith (:) new accum

opposite :: Char -> Char
opposite '0' = '1'
opposite '1' = '0'
opposite _ = error "bad input"

mostCommon :: String -> Char
mostCommon string
  | zeroes > ones = '0'
  | ones > zeroes = '1'
  | otherwise = error "equal number of zeroes and ones"
  where
    zeroes = length $ intersect string "0"
    ones = length $ intersect string "1"

readBinary :: Record -> Int
readBinary = fst . head . readBin

solve :: [Record] -> Int
solve input =
  let cols = foldl update (replicate (length input) []) input
      gamma = fst . head . readBin $ map mostCommon cols
      epsilon = fst . head . readBin $ map (opposite . mostCommon) cols
   in gamma * epsilon

data BitCriteria where
  OxygenGenerator :: BitCriteria
  CO2Scrubber :: BitCriteria
  deriving (Show)

rank :: BitCriteria -> String -> Char
rank criteria string
  | zeroes > ones = case criteria of
    OxygenGenerator -> '0'
    CO2Scrubber -> '1'
  | ones > zeroes = case criteria of
    OxygenGenerator -> '1'
    CO2Scrubber -> '0'
  | zeroes == ones = case criteria of
    OxygenGenerator -> '1'
    CO2Scrubber -> '0'
  | otherwise = error "failed to find rank"
  where
    zeroes = length $ intersect string "0"
    ones = length $ intersect string "1"

solve2 :: BitCriteria -> [Record] -> Record
solve2 criteria input =
  let step :: Int -> [Record] -> Record
      step index rest =
        let bits = map (!! index) rest
            next = filter ((== rank criteria bits) . (!! index)) rest
         in case next of
              [last] -> last
              [] -> error "failed"
              _ -> step (index + 1) next
   in step 0 input

testExample :: TestTree
testExample = testCase "example" $ do
  example <- lines <$> readFile "../input/day3-example.txt"
  solve example @?= 198

testExample2 :: TestTree
testExample2 = testCase "example" $ do
  example <- lines <$> readFile "../input/day3-example.txt"
  solve2 OxygenGenerator example @?= "10111"
  solve2 CO2Scrubber example @?= "01010"

main :: IO ()
main = catch (defaultMain $ testGroup "examples" [testExample, testExample2]) $
  \case
    ExitSuccess -> do
      input <- lines <$> getContents
      -- question 1
      print $ solve input
      -- question 2
      print $
        (readBinary $ solve2 OxygenGenerator input)
          * (readBinary $ solve2 CO2Scrubber input)
    _ -> return ()

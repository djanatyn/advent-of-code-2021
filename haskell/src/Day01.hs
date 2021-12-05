{-# LANGUAGE NamedFieldPuns #-}

module Day01 where

data RelativeDepth where
  RelativeDepth :: {depth :: Int, increase :: Bool} -> RelativeDepth
  deriving (Show)

inputPath :: FilePath
inputPath = "../input/day1.txt"

inputLines :: IO [Int]
inputLines = fmap (read @Int) . lines <$> readFile inputPath

compare :: RelativeDepth -> Int -> RelativeDepth
compare (RelativeDepth {depth, increase}) newDepth =
  RelativeDepth {depth = newDepth, increase = newDepth > depth}

relativeDepths :: [Int] -> [RelativeDepth]
relativeDepths depths =
  scanl Day1.compare (RelativeDepth {depth = head depths, increase = False}) (tail depths)

windows :: [Int] -> [[Int]]
windows depths = let
  measures = take 3 depths
  in case measures of
       [] -> []
       otherwise -> measures : (windows $ tail depths)

main :: IO ()
main = do
  question1 <- relativeDepths <$> inputLines
  -- question 1
  print . length $ filter increase question1
  -- question 2
  question2 <- relativeDepths . fmap sum . windows <$> inputLines
  print . length $ filter increase question2

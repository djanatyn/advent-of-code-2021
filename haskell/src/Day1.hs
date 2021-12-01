{-# LANGUAGE NamedFieldPuns #-}

module Day1 where

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

main :: IO ()
main = do
  result <- relativeDepths <$> inputLines
  print . length $ filter increase result

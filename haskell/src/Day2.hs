{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

import Control.Applicative ((<|>))
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)

inputPath :: FilePath
inputPath = "../input/day2.txt"

inputDirections :: IO [Maybe Direction]
inputDirections = fmap parse . lines <$> readFile inputPath

data Direction where
  Forward :: Int -> Direction
  Down :: Int -> Direction
  Up :: Int -> Direction
  deriving (Show)

data Position where
  Position :: { depth :: Int, horizontal :: Int} -> Position
  deriving (Show)

parse :: String -> Maybe Direction
parse direction =
  (stripPrefix "forward" direction >>= (Just . Forward . read @Int))
    <|> (stripPrefix "down" direction >>= Just . Down . read @Int)
    <|> (stripPrefix "up" direction >>= Just . Up . read @Int)

navigate :: Position -> Direction -> Position
navigate (Position { horizontal, depth }) direction = case direction of
  Forward n -> Position { depth, horizontal = horizontal + n }
  Down n    -> Position { depth = depth + n, horizontal }
  Up n      -> Position { depth = depth - n, horizontal }

main :: IO ()
main = do
  directions <- catMaybes <$> inputDirections
  let end = foldl navigate (Position { depth = 0, horizontal = 0 }) directions in do
    print end
    print $ (depth end) * (horizontal end)

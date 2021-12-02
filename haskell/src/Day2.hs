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
  Position :: { depth :: Int, horizontal :: Int, aim :: Int} -> Position
  deriving (Show)

parse :: String -> Maybe Direction
parse direction =
  (stripPrefix "forward" direction >>= (Just . Forward . read @Int))
    <|> (stripPrefix "down" direction >>= Just . Down . read @Int)
    <|> (stripPrefix "up" direction >>= Just . Up . read @Int)

navigate :: Position -> Direction -> Position
navigate (Position { horizontal, depth, aim }) direction = case direction of
  Forward n -> Position { depth = depth + (aim * n), horizontal = horizontal + n, aim }
  Down n    -> Position { depth, horizontal, aim = aim + n }
  Up n      -> Position { depth, horizontal, aim = aim - n }

main :: IO ()
main = do
  directions <- catMaybes <$> inputDirections
  let end = foldl navigate (Position { depth = 0, horizontal = 0, aim = 0 }) directions in do
    print end
    print $ (depth end) * (horizontal end)

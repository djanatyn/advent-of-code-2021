{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05 where

import Control.Exception (catch)
import Control.Monad (join)
import Data.List
import Data.Maybe (fromJust)
import System.Exit (ExitCode (ExitSuccess))
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.ReadP

newtype Point = Point (Int, Int) deriving (Show, Eq)

data Record where
  Record ::
    { start :: Point,
      finish :: Point
      -- points :: [Point]
    } ->
    Record
  deriving (Show, Eq)

overlapRecord' :: Record -> Point -> Bool
overlapRecord'
  (Record {start = Point (startX, startY), finish = Point (finishX, finishY)})
  (Point (x, y))
    | (startX, startY) == (x, y) = True
    | (finishX, finishY) == (x, y) = True
    -- down
    | startX == finishX && startY > finishY = (x == startX) && (finishY <= y && y <= startY)
    -- up
    | startX == finishX && startY < finishY = (x == startX) && (startY <= y && y <= finishY)
    -- left
    | startY == finishY && startX > startY = (y == startY) && (startX <= x && x <= finishX)
    -- right
    | startY == finishY && startX < startY = (y == startY) && (startX <= x && x <= finishX)
    | otherwise = False

orthogonalLines :: (Point, Point) -> Maybe [Point]
orthogonalLines (Point (p1x, p1y), Point (p2x, p2y))
  -- down
  | p1x == p2x && p1y > p2y = Just $ Point <$> zip (repeat p1x) [p1y, (p1y - 1) .. p2y] -- down
  -- up
  | p1x == p2x && p1y < p2y = Just $ Point <$> zip (repeat p1x) [p1y, (p1y + 1) .. p2y] -- up
  -- left
  | p1y == p2y && p1x > p2x = Just $ Point <$> zip [p1x, (p1x - 1) .. p2x] (repeat p1y) -- left
  -- right
  | p1y == p2y && p1x < p2x = Just $ Point <$> zip [p1x, (p1x + 1) .. p2x] (repeat p1y)
  | otherwise = Nothing

parseRecord :: ReadP (Point, Point)
parseRecord =
  let num = many1 . choice $ char <$> "0123456789"
   in do
        point1x <- num
        char ','
        point1y <- num
        string " -> "
        point2x <- num
        char ','
        point2y <- num

        let point1 = Point (read point1x, read point1y)
            point2 = Point (read point2x, read point2y)
         in return (point1, point2)

buildRecord :: (Point, Point) -> Maybe Record
buildRecord line@(start, finish) = case orthogonalLines line of
  Just points -> Just Record {start, finish}
  Nothing -> Nothing

overlap :: [Record] -> [Point]
overlap records =
  let step :: [Point] -> [Record] -> [Point]
      -- continue
      step accum (current@Record {start, finish} : rest) =
        let checkPoint :: Point -> Bool
            checkPoint point = or $ overlapRecord' <$> (delete current records) <*> [point]

            points :: [Point]
            points = fromJust $ orthogonalLines (start, finish)
         in case filter checkPoint points of
              [] -> step accum rest
              results -> step (nub $ accum ++ results) rest
      -- terminate
      step accum [] = accum
   in step [] records

solve :: [Record] -> Int
solve = undefined

parse :: String -> [Record]
parse = undefined

testExample :: TestTree
testExample = testCase "example" $ do
  parsed <- parse <$> readFile "../input/day5-example.txt"
  solve parsed @?= 5

main :: IO ()
main = catch (defaultMain $ testGroup "examples" [testExample]) $
  \case
    ExitSuccess -> do
      solve . parse <$> readFile "../input/day5.txt" >>= print
    _ -> return ()

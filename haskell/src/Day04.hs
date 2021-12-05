{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day04 where

import Control.Exception (catch)
import Control.Monad (join)
import Data.List (elemIndex, intersect, transpose, (\\))
import Data.Maybe (catMaybes, listToMaybe)
import System.Exit (ExitCode (ExitSuccess))
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.ReadP

data Board where
  Board :: [[Int]] -> Board
  deriving (Show)

newtype Drawings = Drawings [Int]
  deriving (Show)

newtype WinningSequences = WinningSequences [[Int]]
  deriving (Show)

parseRow :: ReadP [String]
parseRow = many1 $ do
  skipSpaces
  (munch1 (\char -> elem char "1234567890"))

parseBoard :: [String] -> Board
parseBoard rows =
  let stringRows = fmap (fst . last . readP_to_S parseRow) rows
   in Board $ fmap (read @Int) <$> stringRows

parseDrawings :: String -> Drawings
parseDrawings input =
  let trim :: [Char] -> ([Char], [Char])
      trim = span (/= ',') . dropWhile (== ',')

      step :: ([Char], [Char]) -> ([Char], [Char])
      step (_, rest) = trim rest
   in Drawings
        . fmap (read @Int)
        . takeWhile (/= "")
        . fmap fst
        . tail
        $ iterate step ([], input)

parse :: String -> (Drawings, [Board])
parse input =
  -- drawings and boards are separaetd with double newlines
  -- afer runnings `lines` this is represented as an empty string
  -- repeatedly apply `span` (via `iterate`) to separate this
  let step :: ([String], [String]) -> ([String], [String])
      step (accum, rest) = span (/= "") (dropWhile (== "") rest)

      results :: [([String], [String])]
      results = tail $ iterate step ([], lines input)

      tokens :: [[String]]
      tokens = takeWhile (/= []) $ fst <$> results
   in case tokens of
        ([header] : boards) -> (parseDrawings header, parseBoard <$> boards)

checkBingo :: Drawings -> Board -> Maybe (Board, WinningSequences)
checkBingo (Drawings drawings) (Board board) =
  let rowsAndCols :: [[Int]]
      rowsAndCols = board ++ transpose board

      matches :: [[Int]]
      matches = intersect drawings <$> rowsAndCols

      winners :: [[Int]]
      winners = filter ((== length board) . length) $ matches
   in case winners of
        [] -> Nothing
        otherwise -> Just (Board board, WinningSequences winners)

data Solution where
  Solution :: {drawings :: [Int], winners :: [(Board, WinningSequences)]} -> Solution
  deriving (Show)

answer :: Solution -> Int
answer Solution {drawings, winners} =
  case winners of
    [(Board rows, (WinningSequences sequences))] ->
      let unmatched = join rows \\ drawings
       in (last drawings) * (sum unmatched)
    otherwise -> error "failed"

solve :: (Drawings, [Board]) -> Solution
solve (Drawings drawings, boards) =
  let step :: Int -> Solution
      step called =
        let calls :: [Int]
            calls = take called drawings

            matches :: [(Board, WinningSequences)]
            matches = catMaybes $ fmap (checkBingo $ Drawings calls) boards
         in case matches of
              [] -> step (called + 1)
              otherwise -> Solution {drawings = calls, winners = matches}
   in step 0

testExample :: TestTree
testExample = testCase "example" $ do
  parsed <- parse <$> readFile "../input/day4-example.txt"
  answer (solve parsed) @?= 4512

main :: IO ()
main = catch (defaultMain $ testGroup "examples" [testExample]) $
  \case
    ExitSuccess -> do
      answer . solve . parse <$> readFile "../input/day4.txt" >>= print
    _ -> return ()

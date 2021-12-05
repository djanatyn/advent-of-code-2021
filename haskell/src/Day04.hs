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
  deriving (Show, Eq)

newtype Drawings = Drawings [Int]
  deriving (Show)

newtype WinningSequences = WinningSequences [[Int]]
  deriving (Show, Eq)

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

solve2 :: (Drawings, [Board]) -> ([Int], [(Board, WinningSequences)])
solve2 (Drawings drawings, boards) =
  let step :: Int -> ([Int], [(Board, WinningSequences)])
      step called =
        let currentCalls :: [Int]
            currentCalls = take called drawings

            matches :: [Int] -> [(Board, WinningSequences)]
            matches calls = catMaybes $ fmap (checkBingo $ Drawings calls) boards
         in case matches currentCalls of
              [] -> step (called + 1)
              otherwise ->
                if (fst <$> matches currentCalls) == boards
                  then (currentCalls, matches currentCalls \\ matches (take (called - 1) drawings))
                  else step (called + 1)
   in step 0

answer2 :: ([Int], [(Board, WinningSequences)]) -> Int
answer2 (drawings, winners) =
  let checkLast :: (Board, WinningSequences) -> Bool
      checkLast (_, WinningSequences seq) = length seq == 1
   in case filter checkLast winners of
        [(Board rows, _)] -> let unmatched = join rows \\ drawings
          in (last drawings) * (sum unmatched)
        otherwise -> error "failed"

testExample :: TestTree
testExample = testCase "example" $ do
  parsed <- parse <$> readFile "../input/day4-example.txt"
  answer (solve parsed) @?= 4512

testExample2 :: TestTree
testExample2 = testCase "example" $ do
  parsed <- parse <$> readFile "../input/day4-example.txt"
  answer2 (solve2 parsed) @?= 1924

main :: IO ()
main = catch (defaultMain $ testGroup "examples" [testExample, testExample2]) $
  \case
    ExitSuccess -> do
      answer . solve . parse <$> readFile "../input/day4.txt" >>= print
      answer2 . solve2 . parse <$> readFile "../input/day4.txt" >>= print
    _ -> return ()

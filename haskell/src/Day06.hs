{-# LANGUAGE LambdaCase #-}

module Day06 where

import Control.Applicative ((<|>))
import Control.Exception (catch)
import Data.List (group, groupBy, sort, sortBy)
import System.Exit (ExitCode (ExitSuccess))
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.ReadP

-- keys are lifespans, values are number of fish
type Generation = (Int, Int)

newtype Ocean = Ocean [Generation] deriving (Show, Eq)

combine :: [Generation] -> [Generation] -> [Generation]
combine a b =
  let match :: Generation -> Generation -> Bool
      match a b = fst a == fst b

      pairSort :: Generation -> Generation -> Ordering
      pairSort a b = compare (fst a) (fst b)

      groupedFish :: [[Generation]]
      groupedFish = groupBy match . sortBy pairSort $ a <> b

      merge :: Generation -> Generation -> Generation
      merge (lifespan, count1) (_, count2) =
        (lifespan, count1 + count2)
   in fmap (foldl1 merge) groupedFish

instance Semigroup Ocean where
  Ocean a <> Ocean b = Ocean $ combine a b

instance Monoid Ocean where
  mempty = Ocean []

next :: Generation -> Ocean
next (0, n) = Ocean [(6, n), (8, n)]
next (lifespan, n) = Ocean [(lifespan - 1, n)]

step :: Ocean -> Ocean
step (Ocean generation) = foldl1 (<>) $ fmap next generation

simulate :: Ocean -> [Ocean]
simulate = iterate step

numFish :: Ocean -> Int
numFish (Ocean generation) = sum $ fmap snd generation

solve :: Int -> Ocean -> Int
solve days =
  numFish
    . (!! days)
    . simulate

parseLanternfish :: ReadP [Int]
parseLanternfish =
  let num :: ReadP Char
      num = choice $ char <$> "0123456789"

      fish :: ReadP Int
      fish = do
        fishLife <- many1 num
        optional $ char ','
        return $ read fishLife
   in many1 fish

parse :: String -> [Int]
parse = fst . last . readP_to_S parseLanternfish

parseOcean :: String -> Ocean
parseOcean input =
  let initialLife :: [[Int]]
      initialLife = group . sort . parse $ input

      collapseGroup :: [Int] -> (Int, Int)
      collapseGroup fish = (head fish, length fish)
   in Ocean $ fmap collapseGroup initialLife

testExample :: TestTree
testExample =
  testCase "example" $
    let solution =
          numFish
            . (!! 80)
            . simulate
            . parseOcean
            $ "3,4,3,1,2"
     in solution @?= 5934

main :: IO ()
main = catch (defaultMain $ testGroup "examples" [testExample]) $
  \case
    ExitSuccess -> do
      day6 <- parseOcean <$> readFile "../input/day6.txt"
      print $ solve 80 day6
      print $ solve 256 day6
    _ -> return ()

module Main (main) where

import System.IO

testSafe :: [Int] -> Bool -> Bool
testSafe [] _ = True
testSafe [x] _ = True
testSafe (x:xs) negative = safety && testSafe xs negative
    where difference = abs (x - head xs)
          safety = difference >= 1 && difference <= 3 && if negative then x > head xs else x < head xs

testSeq :: [Int] -> Bool
testSeq (x:xs) | x > head xs = testSafe (x:xs) True
               | otherwise = testSafe (x:xs) False

pertube :: [a] -> [[a]]
pertube = ptube 0
    where ptube n xs | n >= length xs = [xs]
                     | otherwise = (take n xs ++ drop (n+1) xs) : ptube (n+1) xs

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  rawContents <- hGetContents handle

  -- Part 1
  let contents = map (map read . words) $ lines rawContents :: [[Int]]
      safe = length $ filter id $ map testSeq contents

  putStrLn $ "The number of safe reports is " ++ show safe

  -- Part 2
  let safedampened = length $ filter id $ map (\x -> testSeq x || any testSeq (pertube x)) contents

  putStrLn $ "The number of safe reports with dampening is " ++ show safedampened

  hClose handle

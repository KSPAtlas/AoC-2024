module Main (main) where

import System.IO
import Data.List

countOccurences :: (Eq a) => [a] -> a -> Int
countOccurences [] _ = 0
countOccurences xs f = length ys
    where ys = [x | x <- xs, x == f]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  rawContents <- hGetContents handle

  -- Part 1
  let contents = map words $ lines rawContents
      (leftid, rightid) = (map (read . head) contents, map (read . last) contents)
      sorted_contents = zip (sort leftid) (sort rightid)
      differences = map (abs . uncurry (-)) sorted_contents
      total = sum differences

  putStrLn $ "The total sum is " ++ show total

  -- Part 2
  let occurences = map (countOccurences rightid) leftid
      scores = zipWith (*) leftid occurences
      total2 = sum scores

  putStrLn $ "The similarity score is " ++ show total2
  hClose handle

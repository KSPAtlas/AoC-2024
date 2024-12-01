module Main (main) where

import System.IO
import Data.List

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  rawContents <- hGetContents handle
  let contents = map words $ lines rawContents
      (leftid, rightid) = (map (read . head) contents, map (read . last) contents)
      sorted_contents = zip (sort leftid) (sort rightid)
      differences = map (abs . uncurry (-)) sorted_contents
      total = sum differences

  putStrLn $ "The toal sum is " ++ show total
  hClose handle

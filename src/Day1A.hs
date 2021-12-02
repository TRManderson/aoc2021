module Main where

solve :: [Int] -> Int
solve xs = length . filter (uncurry (<)) $ zip xs (tail xs)

main :: IO ()
main = getContents  >>= (print . solve . map read . lines)

module Main where

solve :: [Int] -> Int
solve xs = length . filter (uncurry (<)) $ zip trio (tail trio)
  where trio = map (\(a, b, c) -> a + b + c) $ zip3 xs (tail xs) (tail . tail $ xs)

main :: IO ()
main = getContents  >>= (print . solve . map read . lines)

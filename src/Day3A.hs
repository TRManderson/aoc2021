module Main where
import Data.List (foldl1', foldl')
import Data.Biapplicative (biliftA2)


cellToPair :: Char -> (Int, Int)
cellToPair '1' = (0, 1)
cellToPair '0' = (1, 0)
cellToPair _ = (0, 0)

doFold :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
doFold = zipWith (biliftA2 (+) (+))

rowToPair :: [(Int, Int)] -> (Int, Int)
rowToPair = foldl' go (0, 0)
  where
    go :: (Int, Int) -> (Int, Int) -> (Int, Int)
    go (a, b) (x, y) = if x > y then (a * 2 + 1, b * 2) else (a * 2, b * 2 + 1)

main = getContents >>= (print . uncurry (*) . rowToPair . foldl1' doFold . fmap (fmap cellToPair) . lines)


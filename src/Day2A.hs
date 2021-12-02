module Main where
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)


parse :: String -> Maybe (Int, Int)
parse = rest . splitOn  " "
  where
    rest (instr:num:_) = case instr of
      "forward" -> Just (read num, 0)
      "up" -> Just (0, read num)
      "down" -> Just (0, -1 * read num)
      _ -> Nothing
    rest _ = Nothing

solve :: [(Int, Int)] -> Int
solve = uncurry (*) . foldr (\(a, b) (c,d) -> (a+c, b+d)) (0, 0)

main :: IO ()
main = getContents  >>= (print . solve . mapMaybe parse . lines)

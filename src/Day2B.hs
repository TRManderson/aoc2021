module Main where
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Foldable (foldl')

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
solve = result . foldl' fn (0, 0, 0)
  where
      fn (pos, dep, aim) (pos', aim') = (pos + pos', dep + (pos' * aim), aim + aim')
      result (pos, dep, _) = pos * dep

main :: IO ()
main = getContents  >>= (print . solve . mapMaybe parse . lines)

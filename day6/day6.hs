import Data.Char (isDigit)
import Data.List.Split (splitOn)

works :: Int -> Int -> Int -> Bool
works dist baseTime remainingTime = dist < remainingTime * speed
  where
    speed = baseTime - remainingTime

ways :: (Int, Int) -> Int
ways (time, dist) = length $ filter id $ map (works dist time) [1 .. (time - 1)]

-- Calculate for [1..(time-1)]
main :: IO ()
main = do
  input <- readFile "./input.txt"
  let lns = lines input
  let [times, dists] :: [[Int]] = map (map read . filter (not . null) . splitOn " " . dropWhile (not . isDigit)) lns
  let result = zipWith (curry ways) times dists
  print $ product result

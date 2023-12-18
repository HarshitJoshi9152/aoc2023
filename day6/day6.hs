import Data.Char (isDigit)
import Data.List.Split (splitOn)

works :: Int -> Int -> Int -> Bool
works dist baseTime remainingTime = dist < remainingTime * speed
  where
    speed = baseTime - remainingTime

waysp2 :: (Int, Int) -> Int
waysp2 (time, dist) = count
  where
    stx = head $ dropWhile (not . works dist time) [1 .. (time - 1)]
    end = time - stx
    count = end - stx + 1

ways :: (Int, Int) -> Int
ways (time, dist) = length $ filter id $ map (works dist time) [1 .. (time - 1)]

-- Calculate for [1..(time-1)]
main :: IO ()
main = do
  input <- readFile "./input.txt"
  let lns = lines input
  -- Part 1 Solution
  let [times, dists] :: [[Int]] = map (map read . filter (not . null) . splitOn " " . dropWhile (not . isDigit)) lns
  let p1 = zipWith (curry waysp2) times dists
  print $ "Part 1 -> " ++ (show . product) p1

  -- Part 2 Solution
  let [time, dist] :: [Int] = map (read . filter isDigit) lns
  let p2 = waysp2 (time, dist)

  print $ "Part 2 -> " ++ show p2

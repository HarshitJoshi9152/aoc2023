import Data.Char (isDigit)
import Data.List.Split (splitOn)

quadraticEquation :: (Integral b, Integral p1, Integral p2) => p1 -> p2 -> b
quadraticEquation d t = ceiling $ (-b + sqrt ((b ** 2) - (4 * a * c))) / (2 * a)
  where
    a = -1
    b = fromIntegral t
    c = fromIntegral (-d)

-- Equation here =>
--    dist = speed * time left
--    d = x * (y - x)
--    d = x * (t - x)
--    d = tx - x^2
--    x^2 - tx + d || -x^2 + tx -d
--    Coefficients =>
--      a = -1
--      b = t
--      c = -d
--    By completing the squares we get
--
--    -b +- sqrt(b^2 - 4ac)
--    --------------------
--             2a

ways :: (Int, Int) -> Int
ways (time, dist) = count
  where
    -- stx = head $ dropWhile (not . works dist time) [1 .. (time - 1)] -- Dumb me
    stx = quadraticEquation dist time
    end = time - stx
    count = end - stx + 1

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let lns = lines input
  -- Part 1 Solution
  let [times, dists] :: [[Int]] = map (map read . filter (not . null) . splitOn " " . dropWhile (not . isDigit)) lns
  let p1 = zipWith (curry ways) times dists
  print $ "Part 1 -> " ++ (show . product) p1

  -- Part 2 Solution
  let [time, dist] :: [Int] = map (read . filter isDigit) lns
  let p2 = ways (time, dist)
  print $ "Part 2 -> " ++ show p2

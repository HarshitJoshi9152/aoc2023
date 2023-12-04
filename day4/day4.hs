import Data.List.Split (splitOn)

matchingCards :: [Int] -> [Int] -> Int
matchingCards win our = length $ filter (`elem` win) our

calScore :: Int -> Int
calScore 1 = 1
calScore matches
  | matches <= 0 = 0
  | otherwise = 2 * calScore (matches - 1)

-- Return score
game :: String -> Int
game str =
  let l = drop 2 $ dropWhile (/= ':') str -- drop 2 chars after too
      parts = splitOn "|" l
      winning_cards :: [Int] = map read $ words $ head parts
      our_cards :: [Int] = map read $ words $ last parts
      matches = matchingCards winning_cards our_cards
   in calScore matches

p1 :: [String] -> Int
p1 str = sum $ map game str

main :: IO ()
main = do
  bytes <- readFile "input.txt"
  let lns = lines bytes
  print ("Part 1 -> " ++ show (p1 lns))
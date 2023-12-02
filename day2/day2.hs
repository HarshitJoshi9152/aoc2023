import Data.Char (isAlpha, isDigit)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- deriving Read means it can directly read "Red" as Red
data Cube = Red | Green | Blue deriving (Eq, Show, Read)

makeCube :: String -> Cube
makeCube "red" = Red
makeCube "green" = Green
makeCube "blue" = Blue
makeCube x = error ("Linkely Invalid Input or bad Parsing\n bad sequence = " ++ x)

parseCube :: String -> (Int, Cube)
parseCube str =
  -- Should i use Data.Text.strip and takeWhile here or break ' ' ?
  let count = read $ filter isDigit str :: Int
      color = filter isAlpha str
      cube = makeCube color
   in (count, cube)

-- An Appearance is a group of cubes taken out at a time
parseAppearance :: String -> [(Int, Cube)]
parseAppearance app =
  let cubes = wordsWhen (== ',') app
   in map parseCube cubes

-- A line is a list of appearances in a single Match
-- parseLine :: String -> [(Int, Cube)]
parseLine :: String -> Bool
parseLine line =
  let l = unwords $ drop 2 $ words line
      rawAppearances = wordsWhen (== ';') l
      -- appearances is a list of all appearances for this line, which itself is a list of (Int, Color) ie [[( int, Color )]]
      appearances = map parseAppearance rawAppearances
   in areAppearancesValid appearances

isCubeValid :: (Int, Cube) -> Bool
isCubeValid (count, Red) = count <= 12
isCubeValid (count, Green) = count <= 13
isCubeValid (count, Blue) = count <= 14

isAppearanceValid :: [(Int, Cube)] -> Bool
isAppearanceValid = all isCubeValid

areAppearancesValid :: [[(Int, Cube)]] -> Bool
areAppearancesValid = all isAppearanceValid

p1 :: [String] -> Int
p1 lns =
  let validIndex = map parseLine lns
      c = zip validIndex [1 ..]
      s = filter fst c
   in sum $ map snd s

-- PART 2

-- Returns MaxColorValues in order (r, g, b) from the list of all appearances in a line
maxColorValues :: [[(Int, Cube)]] -> (Int, Int, Int) -- R, G, B
maxColorValues appearances =
  let flatAppearances = concat appearances
      -- This probably wastes a lot of CPU cycles
      r = maximum $ map fst $ filter (\(_, x) -> x == Red) flatAppearances
      b = maximum $ map fst $ filter (\(_, x) -> x == Blue) flatAppearances
      g = maximum $ map fst $ filter (\(_, x) -> x == Green) flatAppearances
   in (r, g, b)

parseLine2 :: String -> Int
parseLine2 line =
  let l = unwords $ drop 2 $ words line
      rawAppearances = wordsWhen (== ';') l
      -- appearances is a list of all appearances for this line, which itself is a list of (Int, Color) ie [[( int, Color )]]
      appearances = map parseAppearance rawAppearances
      (r, g, b) = maxColorValues appearances
   in r * g * b

p2 :: [String] -> Int
p2 lns = sum $ map parseLine2 lns

main :: IO ()
main = do
  bytes <- readFile "input.txt"
  let lns = lines bytes
  print ("Part 1 -> " ++ show (p1 lns))
  print ("Part 2 -> " ++ show (p2 lns))
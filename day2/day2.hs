import Data.Char (isAlpha, isDigit)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- countSemiColons :: String -> Int
-- countSemiColons line = length $ filter (== ';') line

-- printSemiColons :: [String] -> IO ()
-- printSemiColons lns =
--   let counts = map countSemiColons lns
--    in print counts

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
      -- listOfCubes is a list of all appearances for this line, which itself is a list of (Int, Color) ie [[( int, Color )]]
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

-- PART 2

-- Returns MaxColorValues in order (r, g, b) from the list of all appearances in a line
maxColorValues :: [[(Int, Cube)]] -> (Int, Int, Int) -- R, G, B
maxColorValues appearances =
  let flatAppearances = concat appearances
      r = maximum $ map fst $ filter (\(_, x) -> x == Red) flatAppearances
      b = maximum $ map fst $ filter (\(_, x) -> x == Blue) flatAppearances
      g = maximum $ map fst $ filter (\(_, x) -> x == Green) flatAppearances
   in (r, g, b)

parseLine2 :: String -> Int
parseLine2 line =
  let l = unwords $ drop 2 $ words line
      rawAppearances = wordsWhen (== ';') l
      -- listOfCubes is a list of all appearances for this line, which itself is a list of (Int, Color) ie [[( int, Color )]]
      appearances = map parseAppearance rawAppearances
      (r, g, b) = maxColorValues appearances
   in r * g * b

-- Limit -> 12 red cubes, 13 green cubes, and 14 blue cubes

main :: IO ()
main = do
  bytes <- readFile "input.txt"
  let lns = lines bytes
  let validIndex = map parseLine lns
  let c = zip validIndex [1 .. (length lns)]
  let s = filter fst c
  print $ sum $ map snd s

  -- Part 2

  let powers = map parseLine2 lns

  print $ sum powers
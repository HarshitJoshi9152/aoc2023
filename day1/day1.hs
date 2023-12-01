import Data.Map.Strict (Map, delete, fromList, lookup, member, notMember, size, toList, (!))

numbers = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']

fullParse :: String -> Int
fullParse str = do
  let f_str = filter (`elem` numbers) str
  read [head f_str, last f_str] :: Int

-- Part 2

-- Okay so we need to reduce or fold !
match :: String -> Maybe Int
match ('z' : 'e' : 'r' : 'o' : xs) = Just 0
match ('o' : 'n' : 'e' : xs) = Just 1
match ('t' : 'w' : 'o' : xs) = Just 2
match ('t' : 'h' : 'r' : 'e' : 'e' : xs) = Just 3
match ('f' : 'o' : 'u' : 'r' : xs) = Just 4
match ('f' : 'i' : 'v' : 'e' : xs) = Just 5
match ('s' : 'i' : 'x' : xs) = Just 6
match ('s' : 'e' : 'v' : 'e' : 'n' : xs) = Just 7
match ('e' : 'i' : 'g' : 'h' : 't' : xs) = Just 8
match ('n' : 'i' : 'n' : 'e' : xs) = Just 9
match ('0' : xs) = Just 0
match ('1' : xs) = Just 1
match ('2' : xs) = Just 2
match ('3' : xs) = Just 3
match ('4' : xs) = Just 4
match ('5' : xs) = Just 5
match ('6' : xs) = Just 6
match ('7' : xs) = Just 7
match ('8' : xs) = Just 8
match ('9' : xs) = Just 9
match ('o' : 'r' : 'e' : 'z' : xs) = Just 0
match ('e' : 'n' : 'o' : xs) = Just 1
match ('o' : 'w' : 't' : xs) = Just 2
match ('e' : 'e' : 'r' : 'h' : 't' : xs) = Just 3
match ('r' : 'u' : 'o' : 'f' : xs) = Just 4
match ('e' : 'v' : 'i' : 'f' : xs) = Just 5
match ('x' : 'i' : 's' : xs) = Just 6
match ('n' : 'e' : 'v' : 'e' : 's' : xs) = Just 7
match ('t' : 'h' : 'g' : 'i' : 'e' : xs) = Just 8
match ('e' : 'n' : 'i' : 'n' : xs) = Just 9
match _ = Nothing

-- tried to `match` recursively to input String
try :: String -> String
try [] = error "Length too Short !"
try str = do
  case match str of
    Just val -> show val
    Nothing -> try $ tail str

-- parses twice once for front digit, once for the last digit (By reversing the string)
fullParse2 :: String -> Int
fullParse2 str = do
  let d1 = try str
  let d2 = try $ reverse str
  read (d1 ++ d2) :: Int

main :: IO ()
main = do
  bytes <- readFile "input.txt"
  let lns = lines bytes

  let p1 = sum $ map fullParse lns
  print $ "Part 1 -> " ++ show p1

  let p2 = sum $ map fullParse2 lns
  print $ "Part 2 -> " ++ show p2
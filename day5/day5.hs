import Data.Char (isDigit)
import Data.List.Split (chop, splitOn)
import Data.Maybe (isJust)

seeds :: String -> [Int]
seeds str =
  let seeds = drop 7 str -- Removing "seeds: "
      nums = map read $ words seeds
   in nums

-- ResourceMapping ( d s l )
data RM = RM
  { destinationStart :: Int,
    sourceStart :: Int,
    rangeLength :: Int
  }
  deriving (Read, Eq)

-- If the seed is in range [s, l) we get the destination
rmGet :: RM -> Int -> Maybe Int
rmGet (RM d s l) seed
  | and [seed >= s, seed < s + l] = Just (seed - s + d)
  | otherwise = Nothing

instance Show RM where
  show (RM d s l) = show [d, s, l]

-- TODO: Add mapping title to it as well....
type RMList = [RM]

-- Can i define special Show for RMList, Nope theres a conflict
-- instance Show RMList where
--   show :: RMList -> String
--   show rms =  unlines $ map show rms

printRMList :: RMList -> IO ()
printRMList rms = do
  putStrLn $ unlines $ map show rms

makeRM :: String -> RM
makeRM str =
  let [d, s, l] = map read $ words str
   in RM d s l

-- Goes through all the ResourceMaps of a level and returns the appropriate mapping
getRMListResult :: RMList -> Int -> Int
getRMListResult maps seed
  | null results = seed
  | otherwise = r
  where
    -- I should use dropWhile or something else here that stops when it finds the seed mapping No point in looking over all of them
    results = filter isJust $ map (`rmGet` seed) maps
    (Just r) = head results

-- We will use ti with Chop
parseMap :: [String] -> (RMList, [String])
parseMap [] = error "Invalid Parameters"
parseMap (_ : xs) =
  let (p, left) = span (any isDigit) xs
      m = map makeRM p
   in (m, left)

p1 :: [String] -> Int
p1 = error "Not Implemented"

-- Okay so i can use Chop to parse the rest of the
-- lines
-- Chop () $ tail lns

solvep1 :: [RMList] -> Int -> Int
solvep1 maps s = foldl (flip getRMListResult) s maps

main :: IO ()
main = do
  bytes <- readFile "input.txt"
  let (seedsLine : restLine) = filter (not . null) $ lines bytes
      inputs = seeds seedsLine
      mappings = chop parseMap restLine
      results = map (solvep1 mappings) inputs

  -- map printRMList mappings -- Couldn't match type ‘[]’ with ‘IO’
  -- printRMList (head mappings)
  print ("Part 1 -> " ++ show (minimum results))
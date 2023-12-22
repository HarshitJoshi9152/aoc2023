
import qualified Data.Map.Strict as Map (fromList, lookup, Map, keys)

-- Binary tree search, watchout for circular paths (maybe keep track of the path in the acc ?)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lns = lines contents

    let instructions = head lns

    let hashmap = Map.fromList $ Prelude.map parseMap $ drop 2 lns

    let pathLength = visit hashmap "AAA" 0 instructions

    print ("Part 1 -> " ++ show pathLength)

    -- Part 2

    let starts = filter isStartingPlace $ Map.keys hashmap
    -- let pathLength2 = visitParallel map starts 0 instructions -- BruteForce Wont/Doesnt Work
    let length' = foldl1 lcm $ Prelude.map (\p -> visit hashmap p 0 instructions) starts

    print ("Part 2 -> " ++ show length')


parseMap :: String -> (String, (String, String))
parseMap ln = (k, (left, right))
    where
        w = words ln
        k = head w
        left = take 3 $ drop 1 $ w !! 2
        right= init $ w !! 3

-- Example Input
-- LLR
--
-- AAA = (BBB, BBB)
-- BBB = (AAA, ZZZ)
-- ZZZ = (ZZZ, ZZZ)

-- Call is like so `visit hashmap "AAA" 0 instructions`
visit :: Map.Map String (String, String) -> String -> Int -> String -> Int
visit _ "ZZZ" s _ = s
visit _ [_,_,'Z'] s _ = s       -- For Part 2 
visit hashmap place steps instructions = visit hashmap next_place (steps + 1) instructions
    where
        index = mod steps (length instructions)
        i = instructions !! index -- instruction
        children = case Map.lookup place hashmap of
                        Just a -> a
                        Nothing -> error "Invalid"
        next_place = case i of
            'L' -> fst children
            'R' -> snd children

-- Part 2  (Didnt work)

-- NO NEED TO IMPLEMENT THIS -> foldl1 lcm nums
-- lcm :: [Int] -> Int
-- lcm nums m = 
--     if all (`mod` m') nums then m
--     else lcm nums m+1
--     where m' = (m == 0) ? max nums : m

isStartingPlace :: String -> Bool
isStartingPlace place = last place == 'A'

isEndingPlace :: String -> Bool
isEndingPlace place = 'Z' == last place

visitParallel :: Map.Map String (String, String) -> [String] -> Int -> String -> Int
visitParallel hashmap places steps instructions =
        if foundIt then steps
        else visitParallel hashmap next_places (steps + 1) instructions
    where
        foundIt = all isEndingPlace places -- I think this is whats eating away at our resources...
        children = [child | Just child <- map (`Map.lookup` hashmap) places] -- WTF is CatMaybe
        -- Deciding to go left or right
        index = mod steps (length instructions)
        i = instructions !! index -- instruction
        next_places = case i of
            'L' -> map fst children
            'R' -> map snd children



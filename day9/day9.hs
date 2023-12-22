nextVal :: [Int] -> Int
nextVal seq =
    if all (==0) seq then 0
    else last seq + nextVal (zipWith (-) (drop 1 seq) seq)

nextValp2 :: [Int] -> Int
nextValp2 seq =
    if all (==0) seq then 0
    else head seq - nextValp2 (zipWith (-) (drop 1 seq) seq)

main = do
    contents <- readFile "input.txt"
    let lns = lines contents

    let nums = map (map read . words) lns

    print ("Part 1 -> " ++ show (sum $ map nextVal nums))
    print ("Part 2 -> " ++ show (sum $ map nextValp2 nums))


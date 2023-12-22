import Data.Char (isSpace, isAlphaNum)
import Data.List (sort, group)

data Card = J | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | T | Q | K | A deriving (Eq, Enum, Bounded, Ord);

instance Show Card where
    show TWO = "2"
    show THREE = "3"
    show FOUR = "4"
    show FIVE = "5"
    show SIX = "6"
    show SEVEN = "7"
    show EIGHT = "8"
    show NINE = "9"
    show T = "T"
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"

instance Read Card where
    readsPrec _ c =
        let (s, r) = span (/= ' ') c
        in case c of
             "2" -> [(TWO, r)]
             "3" -> [(THREE, r)]
             "4" -> [(FOUR, r)]
             "5" -> [(FIVE, r)]
             "6" -> [(SIX, r)]
             "7" -> [(SEVEN, r)]
             "8" -> [(EIGHT, r)]
             "9" -> [(NINE, r)]
             "T" -> [(T, r)]
             "J" -> [(J, r)]
             "Q" -> [(Q, r)]
             "K" -> [(K, r)]
             "A" -> [(A, r)]
             _ -> error "no parse"

data Order = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Enum, Bounded, Show, Read, Eq, Ord);

data Deck = Deck {
    cards :: (Card, Card, Card, Card, Card),
    order :: Order,
    score :: Int
    } deriving (Show, Read, Eq);

instance Ord Deck where
    (Deck cards o _) `compare` (Deck cards' o' _) =
        if o == o' then
            cards `compare` cards'
        else
            o `compare` o'


toTuple :: [Card] -> (Card, Card, Card, Card, Card)
toTuple [a, b, c, d, e] = (a, b, c, d, e)

toCard :: String -> Card
toCard str =
    let c :: Card = read str
    in c

-- Haskell List Comprehension is AWESOME 
-- https://wiki.haskell.org/List_comprehension
toCards :: String -> [Card]
toCards str = [ toCard [c] | c <- str, isAlphaNum c]

-- freqToOrder :: [Int] -> Order
-- freqToOrder [5] = FiveOfAKind
-- freqToOrder [4, 1] = FourOfAKind
-- freqToOrder [3, 2] = FullHouse
-- freqToOrder [3, 1, 1] = ThreeOfAKind
-- freqToOrder [2, 2, 1] = TwoPair
-- freqToOrder [2, 1, 1, 1] = OnePair
-- freqToOrder [1, 1, 1, 1, 1] = HighCard
-- freqToOrder _ = error "Not 5 cards"

-- for debugging
freqToOrder :: [Int] -> Order
freqToOrder arr
    | arr == [5] = FiveOfAKind
    | arr == [4, 1] = FourOfAKind
    | arr == [3, 2] = FullHouse
    | arr == [3, 1, 1] = ThreeOfAKind
    | arr == [2, 2, 1] = TwoPair
    | arr == [2, 1, 1, 1] = OnePair
    | arr == [1, 1, 1, 1, 1] = HighCard
    | otherwise = error ("Not 5 cards" ++ show arr)

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

promoteCard :: [Card] -> [Card]
promoteCard [J, J, J, J, J] = [J, J, J, J, J]
promoteCard cards = r
    where
        l = length $ filter (==J) cards
        m = mostCommon cards
        -- For cases like xxJJJ
        m' = case m of 
            J -> mostCommon $ filter (/=J) cards -- take the next mostCommon After J ! (BUT WHAT IF ITS ALL J ?)
            _ -> m
        rest = [c | c <- cards, c /= J]
        r = replicate l m' ++  rest


orderCards :: [Card] -> Order
orderCards cards = freqToOrder freq'
    where
        -- freq = map length $ group $ sort cards -- Part 1 solution
        freq = map length $ group $ sort $ promoteCard cards -- Part 2 solution
        freq' = reverse $ sort freq

parse :: String -> Deck
parse ln = Deck ( toTuple cards ) order score
    where
        w = words ln
        score = read $ last w
        cards :: [Card] = toCards $ head w
        order :: Order = orderCards cards

winnings :: [Deck] -> Int
winnings decks = sum d
    where
        d = zipWith (\a b -> b * score a) decks [1..]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let lns = lines content

    let decks :: [Deck] = map parse lns

    print $ winnings $ sort decks


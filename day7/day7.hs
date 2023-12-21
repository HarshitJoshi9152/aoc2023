import Data.Char (isSpace, isAlphaNum)
import Data.List (sort, group)

data Card = ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | T | J | Q | K | A deriving (Eq, Ord);

instance Show Card where
    show ONE = "1"
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
             "1" -> [(ONE, r)]
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

data Order = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Read, Eq, Ord);

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


freqToOrder :: [Int] -> Order
freqToOrder [5] = FiveOfAKind
freqToOrder [4, 1] = FourOfAKind
freqToOrder [3, 2] = FullHouse
freqToOrder [3, 1, 1] = ThreeOfAKind
freqToOrder [2, 2, 1] = TwoPair
freqToOrder [2, 1, 1, 1] = OnePair
freqToOrder [1, 1, 1, 1, 1] = HighCard
freqToOrder _ = error "Not 5 cards 🤨"

orderCards :: [Card] -> Order
orderCards cards = freqToOrder $ reverse $ sort freq
    where
        freq = map length $ group $ sort cards

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

    -- print decks
    -- print $ sort decks
    print $ winnings $ sort decks


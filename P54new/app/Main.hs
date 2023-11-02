{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
module Main (main) where

import Euler.Helper

import Data.Ord
import Data.List (find, sortOn, zip4)
import Data.Maybe
import Data.Foldable (asum)


data Suit = Diamond
          | Club
          | Heart
          | Spade
          deriving (Show, Enum, Eq, Ord)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Show, Enum, Eq, Ord)

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Show, Eq, Ord)

parseCard :: String -> Card
parseCard s = if length s == 2
              then Card (parseRank (s !! 0)) (parseSuit (s !! 1))
              else errorWithoutStackTrace "parseCard: input string is not length 2"

parseRank :: Char -> Rank
parseRank c =
    case c of
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
      '9' -> Nine
      'T' -> Ten
      'J' -> Jack
      'Q' -> Queen
      'K' -> King
      'A' -> Ace
      _   -> errorWithoutStackTrace "parseRank: invalid rank"

parseSuit :: Char -> Suit
parseSuit c =
    case c of
      'D' -> Diamond
      'C' -> Club
      'H' -> Heart
      'S' -> Spade
      _   -> errorWithoutStackTrace "parseSuit: invalid suit"




sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortOn Down

-- FiveCards is simply an *ordered* lists of five cards.
-- It is not an instance of Ord.
newtype FiveCards = FiveCards { unFiveCards :: [Card]
                              } deriving (Show, Eq)

fromCards :: [Card] -> FiveCards
fromCards l = if length l == 5
              then FiveCards (sortDesc l)
              else errorWithoutStackTrace "fromCards: not a penta"




class Multiple a where
-- A Multiple is n cards of the same rank.
  toRank :: a -> Rank
  toCards :: a -> [Card]


newtype Single = Single { unSingle :: Card
                        } deriving (Show, Eq)
-- A degenerate case

findSingle :: [Card] -> Maybe Single
findSingle cs = Just $ Single (head cs)

instance Multiple Single where
  toRank = rank . unSingle
  toCards (Single c) = [c]

instance Ord Single where
  compare = comparing (rank . unSingle)


newtype Pair = Pair { unPair :: (Rank, (Card, Card))
                    } deriving (Show, Eq)

findPair :: [Card] -> Maybe Pair
findPair cs = fmap toPair . find isPair $ zip cs (tail cs)
  where
    isPair (a, b) = rank a == rank b
    toPair (a, b) = Pair (rank a, (a, b))

instance Multiple Pair where
  toRank = fst . unPair
  toCards (Pair (_, (a, b))) = [a, b]

instance Ord Pair where
  compare = comparing toRank


newtype Triple = Triple { unTriple :: (Rank, (Card, Card, Card))
                        } deriving (Show, Eq)

findTriple :: [Card] -> Maybe Triple
findTriple cs = fmap toTriple . find isTriple $ zip3 cs (tail cs) (drop 2 cs)
  where
    isTriple (a, b, c) = (rank a == rank b) && (rank a == rank c)
    toTriple (a, b, c) = Triple (rank a, (a, b, c))

instance Multiple Triple where
  toRank = fst . unTriple
  toCards (Triple (_, (a, b, c))) = [a, b, c]

instance Ord Triple where
  compare = comparing toRank


newtype Quad = Quad { unQuad :: (Rank, (Card, Card, Card, Card))
                    } deriving (Show, Eq)

findQuad :: [Card] -> Maybe Quad
findQuad cs = fmap toQuad . find isQuad $ zip4 cs (tail cs) (drop 2 cs) (drop 3 cs)
  where
    isQuad (a, b, c, d) = (rank a == rank b) && (rank a == rank c) && (rank a == rank d)
    toQuad (a, b, c, d) = Quad (rank a, (a, b, c, d))

instance Multiple Quad where
  toRank = fst . unQuad
  toCards (Quad (_, (a, b, c, d))) = [a, b, c, d]

instance Ord Quad where
  compare = comparing toRank




newtype Kicker = Kicker { unKicker :: [Card]
                        } deriving (Show, Eq)

instance Ord Kicker where
  compare = comparing (map rank . unKicker)


-- Two options:
-- 1. TypeFamilies
--      Pros:
--      - comparing prime `whenEQ` comparing kicker
--      - Having a prime function which extracts the "important" part of a hand
--      Cons:
--      - Requires TypeFamilies
-- 2. Simple sum type
--      Pros:
--      - Simple
--      Cons:
--      - Complicated Ord instance
--      - No prime function
--
-- What does it take to write something like
-- "comparing prime `whenEQ` comparing kicker"?
-- Hand must have an instance of Ord so Hand is a concrete type not type
-- constructor
-- prime :: Hand -> _
-- prime has no valid return type.
-- On second thought prime is a useless function.
-- prime can have a return type of Prime, a tagged union of all the possible
-- return types.
-- There is no meaningful operation that can be operated on the union, e.g.
-- there is no meaningful function that does something to a Single and a Pair.


data Hand = HighCard Single Kicker
          | OnePair Pair Kicker
          | TwoPairs Pair Pair Kicker
          | ThreeOfAKind Triple Kicker
          | Straight Rank [Card]
          | Flush Suit [Card]
          | FullHouse Triple Pair
          | FourOfAKind Quad Single
          | StraightFlush Rank [Card]
          deriving (Show, Eq)

ifEQ :: Ordering -> Ordering -> Ordering
ifEQ a b = case a of
    EQ -> b
    _  -> a
infixr 9 `ifEQ`

instance Ord Hand where
  compare (HighCard s1 k1) (HighCard s2 k2) =
      compare s1 s2 `ifEQ` compare k1 k2
  compare (OnePair p1 k1) (OnePair p2 k2) =
      compare p1 p2 `ifEQ` compare k1 k2
  compare (TwoPairs pa1 pb1 k1) (TwoPairs pa2 pb2 k2) =
      compare pa1 pa2 `ifEQ` compare pb1 pb2 `ifEQ` compare k1 k2
  compare (ThreeOfAKind t1 k1) (ThreeOfAKind t2 k2) =
      compare t1 t2 `ifEQ` compare k1 k2
  compare (Straight r1 _) (Straight r2 _) =
      compare r1 r2
  compare (Flush s1 f1) (Flush s2 f2) =
      compare s1 s2 `ifEQ` comparing (map rank) f1 f2
  compare (FullHouse t1 p1) (FullHouse t2 p2) =
      compare t1 t2 `ifEQ` compare p1 p2
  compare (FourOfAKind q1 s1) (FourOfAKind q2 s2) =
      compare q1 q2 `ifEQ` compare s1 s2
  compare (StraightFlush r1 _) (StraightFlush r2 _) =
      compare r1 r2
  compare a b =
      comparing handToInt a b

handToInt :: Hand -> Int
handToInt HighCard {}      = 0
handToInt OnePair {}       = 1
handToInt TwoPairs {}      = 2
handToInt ThreeOfAKind {}  = 3
handToInt Straight {}      = 4
handToInt Flush {}         = 5
handToInt FullHouse {}     = 6
handToInt FourOfAKind {}   = 7
handToInt StraightFlush {} = 8




-- Every FiveCards is at the very least a high card.
toHighCard :: FiveCards -> Hand
toHighCard (FiveCards (p:ps)) = HighCard (Single p) (Kicker ps)
toHighCard _ = errorWithoutStackTrace "toHighCard: empty FiveCards"


toOnePair :: FiveCards -> Maybe Hand
toOnePair p = do
    pair <- findPair cs
    let remaining = cs `deleteOrdR` toCards pair
    return $ OnePair pair (Kicker remaining)
  where
    cs = unFiveCards p


toTwoPair :: FiveCards -> Maybe Hand
toTwoPair p = do
    pair1 <- findPair cs
    let remaining1 = cs `deleteOrdR` toCards pair1
    pair2 <- findPair remaining1
    let remaining2 = remaining1 `deleteOrdR` toCards pair2
    return $ TwoPairs pair1 pair2 (Kicker remaining2)
  where
    cs = unFiveCards p


toThreeOfAKind :: FiveCards -> Maybe Hand
toThreeOfAKind p = do
    triple <- findTriple cs
    let remaining = cs `deleteOrdR` toCards triple
    return $ ThreeOfAKind triple (Kicker remaining)
  where
    cs = unFiveCards p


checkStraight :: FiveCards -> Maybe Rank
checkStraight p = if isStraight
                  then Just $ rank ch
                  else Nothing
  where
    cs@(ch:_) = unFiveCards p
    isStraight =
        map rankToInt cs == enumFromThenTo headAsInt (headAsInt - 1) (headAsInt - 4)
    rankToInt = fromEnum . rank
    headAsInt = rankToInt ch
{-# INLINE checkStraight #-}

toStraight :: FiveCards -> Maybe Hand
toStraight p = do
    rk <- checkStraight p
    return $ Straight rk cs
  where
    cs = unFiveCards p


checkFlush :: FiveCards -> Maybe Suit
checkFlush p = if isFlush
               then Just $ suit ch
               else Nothing
  where
    isFlush = all (\c -> suit c == suit ch) ct
    cs@(ch:ct) = unFiveCards p
{-# INLINE checkFlush #-}

toFlush :: FiveCards -> Maybe Hand
toFlush p = do
    st <- checkFlush p
    return $ Flush st cs
  where
    cs = unFiveCards p


toFullHouse :: FiveCards -> Maybe Hand
toFullHouse p = do
    triple <- findTriple cs
    let remaining1 = cs `deleteOrdR` toCards triple
    pair <- findPair remaining1
    return $ FullHouse triple pair
  where
    cs = unFiveCards p


toFourOfAKind :: FiveCards -> Maybe Hand
toFourOfAKind p = do
    quad <- findQuad cs
    let remaining = cs `deleteOrdR` toCards quad
    return $ FourOfAKind quad (fromJust $ findSingle remaining)
  where
    cs = unFiveCards p


toStraightFlush :: FiveCards -> Maybe Hand
toStraightFlush p = if isStraight && isFlush
                    then Just $ StraightFlush (rank ch) cs
                    else Nothing
  where
    cs@(ch:ct) = unFiveCards p
    isStraight = map rankToInt cs
      == enumFromThenTo headAsInt (headAsInt - 1) (headAsInt - 4)
      where
        rankToInt = fromEnum . rank
        headAsInt = rankToInt ch
    isFlush = all (\ x -> suit x == suit ch) ct


bigParser :: FiveCards -> Hand
bigParser x = fromMaybe (toHighCard x) (asum listOfParser)
  where
    listOfParser = map ($ x)
                     [ toStraightFlush
                     , toFourOfAKind
                     , toFullHouse
                     , toFlush
                     , toStraight
                     , toThreeOfAKind
                     , toTwoPair
                     , toOnePair
                     ]

toHand :: [String] -> Hand
toHand = bigParser . fromCards . map parseCard

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)
{-# INLINE mapBoth #-}




main :: IO ()
main = do
    file <- readFile "p054_poker.txt"
    let games = (map (splitAt 5 . words) . lines) file
        hands = map (mapBoth toHand) games
    print $ length $ filter (== GT) (map (uncurry compare) hands)

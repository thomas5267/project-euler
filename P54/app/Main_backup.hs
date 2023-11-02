{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Euler.Helper

import Data.Ord
import Data.List -- (find, sort, sortOn)
import Data.Maybe


data Suit = Diamond | Club | Heart | Spade deriving (Show, Enum, Eq, Ord)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Show, Enum, Eq, Ord)

newtype Card = Card { getCard :: (Rank, Suit) } deriving (Show, Eq, Ord)

parseCard :: String -> Card
parseCard s = if length s == 2
              then Card (parseRank (s !! 0), parseSuit (s !! 1))
              else errorWithoutStackTrace "toCard: input string is not length 2"

rank :: Card -> Rank
rank = fst . getCard

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
      _   -> errorWithoutStackTrace "toRank: invalid rank"

suit :: Card -> Suit
suit = snd . getCard

parseSuit :: Char -> Suit
parseSuit c =
    case c of
      'D' -> Diamond
      'C' -> Club
      'H' -> Heart
      'S' -> Spade
      _   -> errorWithoutStackTrace "toRank: invalid suit"


class Cards a where
-- If a is a collection of Card, then it belongs to Cards.
  toCards :: a -> [Card]
  fromCards :: [Card] -> a

class Multiple a where
-- A Multiple is n cards of the same rank.
  toRank :: a -> Rank
  findMultiple :: [Card] -> Maybe a


isSameRank :: [Card] -> Bool
isSameRank l = all (\x -> rank x == rank (head l)) l

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortOn Down

-- FiveCards is simply an ordered lists of five cards.
-- It is also not an instance of Ord.
newtype FiveCards = FiveCards [Card]  deriving (Show, Eq)

instance Cards FiveCards where
  toCards (FiveCards l) = l
  fromCards l = if length l == 5
                then FiveCards (sortDesc l)
                else errorWithoutStackTrace "fromCards: not a penta"


-- class (Ord a) => Prime a




newtype Single = Single Card deriving (Show, Eq)
-- A degenerate case
instance Cards Single where
  toCards (Single p) = [p]
  fromCards l = if length l == 1
                then Single (head l)
                else errorWithoutStackTrace "fromCards: not a one element list"

-- instance Prime Single

instance Multiple Single where
  toRank (Single c) = rank c
  findMultiple l = Just $ Single (head l)

instance Ord Single where
  compare (Single a) (Single b) = comparing rank a b


newtype Pair = Pair (Rank, [Card]) deriving (Show, Eq)

-- instance Prime Pair

instance Cards Pair where
  toCards (Pair p) = snd p
  fromCards l = if length l == 2
                then fromMaybe (errorWithoutStackTrace "fromCards: not a pair") (findMultiple l)
                else errorWithoutStackTrace "fromCards: not a two element list"

instance Multiple Pair where
  toRank (Pair p) = fst p
  findMultiple l = (fmap toPair . find isPair) (zip sl (tail sl))
    where
      sl = sortDesc l
      isPair (a, b) = rank a == rank b
      toPair (a, b) = Pair (rank a, [a, b])

instance Ord Pair where
  compare a b = comparing toRank a b


newtype Triple = Triple (Rank, [Card]) deriving (Show, Eq)

-- instance Prime Triple

instance Cards Triple where
  toCards (Triple t) = snd t
  fromCards l = if length l == 3
                then fromMaybe (errorWithoutStackTrace "fromCards: not a triple") (findMultiple l)
                else errorWithoutStackTrace "fromCards: not a three element list"

instance Multiple Triple where
  toRank (Triple t) = fst t
  findMultiple l = (fmap toTriple . find isTriple) (zip3 sl (tail sl) (drop 2 sl))
    where
      sl = sortDesc l
      isTriple (a, b, c) = (rank a == rank b) && (rank a == rank c)
      toTriple (a, b, c) = Triple (rank a, [a, b, c])

instance Ord Triple where
  compare a b = comparing toRank a b


newtype Quad = Quad (Rank, [Card]) deriving (Show, Eq)

-- instance Prime Quad

instance Cards Quad where
  toCards (Quad q) = snd q
  fromCards l = if length l == 4
                then fromMaybe (errorWithoutStackTrace "fromCards: not a quad") (findMultiple l)
                else errorWithoutStackTrace "fromCards: not a four element list"

instance Multiple Quad where
  toRank (Quad q) = fst q
  findMultiple l = (fmap toQuad . find isQuad) (zip4 sl (tail sl) (drop 2 sl) (drop 3 sl))
    where
      sl = sortDesc l
      isQuad (a, b, c, d) = (rank a == rank b) && (rank a == rank c) && (rank a == rank d)
      toQuad (a, b, c, d) = Quad (rank a, [a, b, c, d])

instance Ord Quad where
  compare a b = comparing toRank a b




-- class (Ord a) => Kicker a

-- instance Kicker ()
-- Some combinations have no kicker

newtype KSingle = KSingle Card deriving (Show, Eq)

-- instance Kicker KSingle

instance Cards KSingle where
  toCards (KSingle c) = [c]
  fromCards l = if length l == 1
                then KSingle (head l)
                else errorWithoutStackTrace "fromCards: not a kicker single"

instance Ord KSingle where
  compare (KSingle a) (KSingle b) = comparing rank a b


newtype KPair = KPair [Card] deriving (Show, Eq)

-- instance Kicker KPair

instance Cards KPair where
  toCards (KPair l) = l
  fromCards l = if length l == 2
                then KPair (sortDesc l)
                else errorWithoutStackTrace "fromCards: not a kicker pair"

instance Ord KPair where
  compare a b = comparing (map rank) (toCards a) (toCards b)


newtype KTriple = KTriple [Card] deriving (Show, Eq)

-- instance Kicker KTriple

instance Cards KTriple where
  toCards (KTriple l) = l
  fromCards l = if length l == 3
                then KTriple (sortDesc l)
                else errorWithoutStackTrace "fromCards: not a kicker triple"

instance Ord KTriple where
  compare a b = comparing (map rank) (toCards a) (toCards b)


newtype KQuad = KQuad [Card] deriving (Show, Eq)

-- instance Kicker KQuad

instance Cards KQuad where
  toCards (KQuad l) = l
  fromCards l = if length l == 4
                then KQuad (sortDesc l)
                else errorWithoutStackTrace "fromCards: not a kicker quad"

instance Ord KQuad where
  compare a b = comparing (map rank) (toCards a) (toCards b)




class (Ord (Prime v), Ord (Kicker v)) => ValidHand v where
  type Prime v
  type Kicker v
  prime :: v -> Prime v
  kicker :: v -> Kicker v

compareValidHand :: (ValidHand v) => v -> v -> Ordering
compareValidHand a b =
    case comparing prime a b of
      GT -> GT
      LT -> LT
      EQ -> case comparing kicker a b of
              GT -> GT
              LT -> LT
              EQ -> EQ


newtype HighCard = HighCard (Single, KQuad) deriving (Show, Eq)

instance ValidHand HighCard where
  type Prime HighCard = Single
  type Kicker HighCard = KQuad
  prime (HighCard p) = fst p
  kicker (HighCard p) = snd p

instance Ord HighCard where
  compare a b = compareValidHand a b


newtype OnePair = OnePair (Pair, KTriple) deriving (Show, Eq)

instance ValidHand OnePair where
  type Prime OnePair = Pair
  type Kicker OnePair = KTriple
  prime (OnePair p) = fst p
  kicker (OnePair p) = snd p

instance Ord OnePair where
  compare a b = compareValidHand a b


newtype TwoPairs = TwoPairs ((Pair, Pair), KSingle) deriving (Show, Eq)

instance ValidHand TwoPairs where
  type Prime TwoPairs = (Pair, Pair)
  type Kicker TwoPairs = KSingle
  prime (TwoPairs p) = fst p
  kicker (TwoPairs p) = snd p

instance Ord TwoPairs where
  compare a b = compareValidHand a b


newtype ThreeOfAKind = ThreeOfAKind (Triple, KPair) deriving (Show, Eq)

instance ValidHand ThreeOfAKind where
  type Prime ThreeOfAKind = Triple
  type Kicker ThreeOfAKind = KPair
  prime (ThreeOfAKind p) = fst p
  kicker (ThreeOfAKind p) = snd p

instance Ord ThreeOfAKind where
  compare a b = compareValidHand a b


newtype Straight = Straight (Rank, FiveCards) deriving (Show, Eq)

instance ValidHand Straight where
  type Prime Straight = Rank
  type Kicker Straight = ()
  prime (Straight p) = fst p
  kicker _ = ()

instance Ord Straight where
-- compareValidHand still works but () is always EQ so there is no point.
  compare a b = comparing prime a b


newtype Flush = Flush (Suit, FiveCards) deriving (Show, Eq)

instance ValidHand Flush where
  type Prime Flush = Suit
  type Kicker Flush = ()
  prime (Flush p) = fst p
  kicker _ = ()

instance Ord Flush where
  compare a b = comparing prime a b


newtype FullHouse = FullHouse (Triple, Pair) deriving (Show, Eq)

instance ValidHand FullHouse where
  type Prime FullHouse = Triple
  type Kicker FullHouse = Pair
  prime (FullHouse p) = fst p
  kicker (FullHouse p) = snd p

instance Ord FullHouse where
  compare a b = compareValidHand a b


newtype FourOfAKind = FourOfAKind (Quad, KSingle) deriving (Show, Eq)

instance ValidHand FourOfAKind where
  type Prime FourOfAKind = Quad
  type Kicker FourOfAKind = KSingle
  prime (FourOfAKind p) = fst p
  kicker (FourOfAKind p) = snd p

instance Ord FourOfAKind where
  compare a b = compareValidHand a b


newtype StraightFlush = StraightFlush (Rank, FiveCards) deriving (Show, Eq)

instance ValidHand StraightFlush where
  type Prime StraightFlush = Rank
  type Kicker StraightFlush = ()
  prime (StraightFlush p) = fst p
  kicker _ = ()

instance Ord StraightFlush where
  compare a b = compareValidHand a b

data Hand = forall a. (ValidHand a) => Hand a

handToInt :: Hand -> Int
handToInt (Hand (HighCard      _ )) = 0
handToInt (Hand (OnePair       _)) = 1
handToInt (Hand (TwoPairs      _)) = 2
handToInt (Hand (ThreeOfAKind  _)) = 3
handToInt (Hand (Straight      _)) = 4
handToInt (Hand (Flush         _)) = 5
handToInt (Hand (FullHouse     _)) = 6
handToInt (Hand (FourOfAKind   _)) = 7
handToInt (Hand (StraightFlush _)) = 8

instance Ord Hand where
  compare a b =
      case comparing handToInt a b of
        GT -> GT
        LT -> LT
        EQ -> comparing fromHand a b

-- data Hand = HHighCard HighCard
--           | HOnePair OnePair
--           | HTwoPairs TwoPairs
--           | HThreeOfAKind ThreeOfAKind
--           | HStraight Straight
--           | HFlush Flush
--           | HFullHouse FullHouse
--           | HFourOfAKind FourOfAKind
--           | HStraightFlush StraightFlush
--           deriving (Show, Eq)

-- data Hand a where
--   HHighCard      :: HighCard -> Hand HighCard
--   HOnePair       :: OnePair -> Hand OnePair
--   HTwoPairs      :: TwoPairs -> Hand TwoPairs
--   HThreeOfAKind  :: ThreeOfAKind -> Hand ThreeOfAKind
--   HStraight      :: Straight -> Hand Straight
--   HFlush         :: Flush -> Hand Flush
--   HFullHouse     :: FullHouse -> Hand FullHouse
--   HFourOfAKind   :: FourOfAKind -> Hand FourOfAKind
--   HStraightFlush :: StraightFlush -> Hand StraightFlush

-- fromHand :: ValidHand a => Hand -> a
-- fromHand (HHighCard      a) = a
-- fromHand (HOnePair       a) = a
-- fromHand (HTwoPairs      a) = a
-- fromHand (HThreeOfAKind  a) = a
-- fromHand (HStraight      a) = a
-- fromHand (HFlush         a) = a
-- fromHand (HFullHouse     a) = a
-- fromHand (HFourOfAKind   a) = a
-- fromHand (HStraightFlush a) = a
-- 
-- handToInt :: Hand -> Int
-- handToInt (HHighCard      _) = 0
-- handToInt (HOnePair       _) = 1
-- handToInt (HTwoPairs      _) = 2
-- handToInt (HThreeOfAKind  _) = 3
-- handToInt (HStraight      _) = 4
-- handToInt (HFlush         _) = 5
-- handToInt (HFullHouse     _) = 6
-- handToInt (HFourOfAKind   _) = 7
-- handToInt (HStraightFlush _) = 8

-- instance Ord Hand where
--   compare a b =
--       case comparing handToInt a b of
--         GT -> GT
--         LT -> LT
--         EQ -> comparing fromHand a b
-- 
toHighCard :: FiveCards -> Maybe Hand
toHighCard (FiveCards p) = Just $ Hand $ HighCard (fromCards (take 1 p), fromCards (tail p))
-- 
-- toOnePair :: FiveCards -> Maybe Hand
-- toOnePair p = do
--     pair <- findMultiple (toCards p)
--     let remaining = toCards p `deleteOrd` toCards pair
--     return $ OnePair pair (KTriple remaining)

-- toTwoPair :: FiveCards -> Maybe Hand
-- toTwoPair p = do
--     pair1 <- findMultiple (toCards p)
--     let remaining1 = toCards p `deleteOrd` toCards pair1
--     pair2 <- findMultiple remaining1
--     let remaining2 = remaining1 `deleteOrd` toCards pair2
--     return $ TwoPairs pair1 pair2 (head remaining2)
-- 
-- toThreeOfAKind :: FiveCards -> Maybe Hand
-- toThreeOfAKind p = do
--     triple <- findMultiple (toCards p)
--     let remaining = toCards p `deleteOrd` toCards triple
--     return $ ThreeOfAKind triple (KPair remaining)
-- 
-- checkStraight :: FiveCards -> Bool
-- checkStraight p =
--     map rankToInt pl == enumFromThenTo headAsInt (headAsInt - 1) (headAsInt - 4)
--   where
--     pl = toCards p
--     rankToInt = fromEnum . rank
--     headAsInt = rankToInt (head pl)
-- 
-- toStraight :: FiveCards -> Maybe Hand
-- toStraight p = if checkStraight p
--                then Just $ Straight (rank (head pl)) p
--                else Nothing
--   where
--     pl = toCards p
-- 
-- checkFlush :: FiveCards -> Bool
-- checkFlush p = all (\c -> suit c == suit (head pl)) pl
--   where pl = toCards p
-- 
-- toFlush :: FiveCards -> Maybe Hand
-- toFlush p = if checkFlush p
--             then Just $ Flush (suit (head pl)) p
--             else Nothing
--   where
--     pl = toCards p
-- 
-- toFullHouse :: FiveCards -> Maybe Hand
-- toFullHouse p = do
--     triple <- findMultiple (toCards p)
--     let remaining1 = toCards p `deleteOrd` toCards triple
--     pair <- findMultiple remaining1
--     return $ FullHouse triple pair
-- 
-- toFourOfAKind :: FiveCards -> Maybe Hand
-- toFourOfAKind p = do
--     quad <- findMultiple (toCards p)
--     let remaining = toCards p `deleteOrd` toCards quad
--     return $ FourOfAKind quad (head remaining)
-- 
-- toStraightFlush :: FiveCards -> Maybe Hand
-- toStraightFlush p = if checkStraight p && checkFlush p
--                     then Just $ StraightFlush ((rank . head) (toCards p)) p
--                     else Nothing

main :: IO ()
main = print "hello"

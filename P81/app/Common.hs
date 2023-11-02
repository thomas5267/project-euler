module Common where

import Data.Array.Unboxed
import Data.List.Split
import qualified Data.PSQueue as Q
import qualified Data.Set as S

type Matrix = UArray (Int, Int) Int

parseToList :: String -> [[Int]]
parseToList = map (map read . splitOn ",") . lines

type Bound = ((Int, Int), (Int, Int))

parseToArray :: Bound -> [[Int]] -> Matrix
parseToArray bound l =
    accumArray (+) 0 bound . concat $ zipWith zip pairs l
  where
    pairs :: [[(Int, Int)]]
    pairs = [ [ (r, c) | c <- [0..] ] | r <- [0..] ]

testMatrix :: Matrix
testMatrix = parseToArray ((0, 0), (4, 4)) testList
  where
    testList = [[131, 673, 234, 103,  18],
               [201,  96, 342, 965, 150],
               [630, 803, 746, 422, 111],
               [537, 699, 497, 121, 956],
               [805, 732, 524,  37, 331]]


data Weight = Infinity | Weight Int deriving (Show, Eq)

instance Ord Weight where
  compare Infinity   Infinity   = EQ
  compare Infinity   _          = GT
  compare _          Infinity   = LT
  compare (Weight a) (Weight b) = compare a b


type Vertex = (Int, Int)

type Adjacent = Bound -> Vertex -> S.Set Vertex
type Starter = Matrix -> [(Vertex, Weight)]
type Stopper = Vertex -> Bool

decrease :: (Ord k, Ord p) => (k, p) -> Q.PSQ k p -> Q.PSQ k p
decrease (k, p) = Q.adjust (min p) k

decrease_list :: (Ord k, Ord p) => [(k, p)] -> Q.PSQ k p -> Q.PSQ k p
decrease_list bs q = foldr decrease q bs


dijkstra :: Matrix -> Adjacent -> Starter -> Stopper -> [(Vertex, Int)]
dijkstra m adj start stop = loop (decrease_list (start m) q0) S.empty
  where
    bound@((r0, c0), (rm, cm)) = bounds m
    q0 = Q.fromAscList [ (x, y) Q.:-> Infinity | x <- [r0..rm], y <- [c0..cm] ]
    loop :: Q.PSQ Vertex Weight -> S.Set Vertex -> [(Vertex, Int)]
    loop q s =
        case Q.minView q of
          Nothing -> []

          Just (u Q.:-> Weight d, qn) ->
              (u, d) : if stop u then [] else loop (decrease_list bs qn) (S.insert u s)
            where
              bs :: [(Vertex, Weight)]
              bs = foldr ((:) . \v -> (v, Weight (d + m ! v))) [] (adj bound u `S.difference` s)

          Just (_ Q.:-> Infinity, _)  ->
              errorWithoutStackTrace "dijkstra: end vertex not reachable"

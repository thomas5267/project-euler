module Main where

import qualified Data.Array as A
import qualified Data.Graph as G

import Control.Applicative
import Control.Monad

import Control.Monad.State
import Control.Monad.Reader

triangle :: Int -> Int
triangle n = n*(n-1) `quot` 2

square :: Int -> Int
square n = n^(2 :: Int)

pentagon :: Int -> Int
pentagon n = n*(3*n - 1) `quot` 2

hexagon :: Int -> Int
hexagon n = n*(2*n - 1)

heptagon :: Int -> Int
heptagon n = n*(5*n - 3) `quot` 2

octagon :: Int -> Int
octagon n = n*(3*n - 2)

-- figurate :: Int -> Int -> Int
-- figurate 3 = triangle
-- figurate 4 = square
-- figurate 5 = pentagon
-- figurate 6 = hexagon
-- figurate 7 = heptagon
-- figurate 8 = octagon
-- figurate n = error ("figurate: " ++ show n ++ "-gon not an implemented shape")

-- We'll do this problem by graph.
-- Let the figurate numbers be the vertices of a graph.
-- There is a directed edge from a to b if the last two digits of a
-- is equal to the first two digits of b.
-- Then we find cycles in the graph.

lowerBound :: Int
lowerBound = 1000

upperBound :: Int
upperBound = 9999

-- We will encode which figure the number belongs
-- using the hundred thousands place.
-- An extra zero in between provides an obvious sign should it overflow.

figurePlace :: Int
figurePlace = 100000

lowerBoundGraph :: Int
lowerBoundGraph = 3*figurePlace + 1000

upperBoundGraph :: Int
upperBoundGraph = 8*figurePlace + 9999

figurateGraph :: Int -> Int -> G.Vertex
figurateGraph 3 k = triangle k + 3*figurePlace
figurateGraph 4 k = square k   + 4*figurePlace
figurateGraph 5 k = pentagon k + 5*figurePlace
figurateGraph 6 k = hexagon k  + 6*figurePlace
figurateGraph 7 k = heptagon k + 7*figurePlace
figurateGraph 8 k = octagon k  + 8*figurePlace
figurateGraph n _ = error ("figurateGraph: " ++ show n ++ "-gon not an implemented shape")

vertices :: Int -> [G.Vertex]
-- Returns a list of n-gon numbers with the first two digit encoding the type.
vertices n = (takeWhile (<=upperBound + n*figurePlace) . dropWhile (<=lowerBound + n*figurePlace))
             (map (figurateGraph n) [1..])

firstTwoDigits :: G.Vertex -> G.Vertex
-- As we used the hundred thousands place to encode which figure it is from,
-- firstTwoDigits actually returns the third and fourth digit of a number.
firstTwoDigits n = n `rem` figurePlace `quot` 100
{-# INLINE firstTwoDigits #-}

lastTwoDigits :: G.Vertex -> G.Vertex
lastTwoDigits n = n `rem` 100
{-# INLINE lastTwoDigits #-}

hasEdge :: G.Vertex -> G.Vertex -> [G.Edge]
-- Returns the edges *between* vertex a and vertex b.
-- Edges in both direction are found.
hasEdge a b = do
    let temp1 = if lastTwoDigits a == firstTwoDigits b then return (a, b) else []
        temp2 = if lastTwoDigits b == firstTwoDigits a then return (b, a) else []
    temp1 ++ temp2

findEdges :: G.Vertex -> G.Vertex -> [G.Edge]
-- Find all the edges *between* a-gon numbers and b-gon numbers.
-- Edges in both direction are found.
findEdges a b = join $ liftA2 hasEdge (vertices a) (vertices b)

edgeList :: [[G.Edge]]
edgeList = [ findEdges a b | a <- [3..7], b <- [(a+1)..8] ]

bestGraph :: G.Graph
bestGraph = G.buildG (lowerBoundGraph, upperBoundGraph) (join edgeList)

vertexList :: [G.Vertex]
vertexList = join $ map vertices [3..8]

type FigureType = Int

toFigure :: G.Vertex -> FigureType
-- Given a vertex finds which figure it is from.
toFigure n = n `quot` figurePlace

type Path = [G.Vertex]

nReachable :: G.Graph -> Int -> G.Vertex -> [Path]
-- Finds the vertices reachable from v with a path of exactly length n.
nReachable g = nReachable'
  where
    nReachable' 0 v = [[v]]
    nReachable' n v = map (v:) (concatMap (nReachable' (n-1)) (edges v))
      where edges = (g A.!)

reachableRestricted :: G.Graph -> [FigureType] -> G.Vertex -> [G.Vertex]
-- Returns a list of vertices directly reachable from v,
-- but only if the vertex is not one of the figurate types in l.
reachableRestricted g = reachableRestricted'
  where
    reachableRestricted' l v = filter ((`notElem` l) . toFigure) (edges v)
      where edges = (g A.!)

nReachableRestricted :: G.Graph -> Int -> G.Vertex -> [Path]
-- Finds the vertices reachable from v with a path of exactly length n
-- with the restriction that no vertices of the same figure type can be in the path.
-- Returns the path taken.
nReachableRestricted g = nReachableRestricted' []
  where
    nReachableRestricted' :: [FigureType] -> Int -> G.Vertex -> [Path]
    nReachableRestricted' _ 0 v = [[v]]
    nReachableRestricted' l n v =
        map (v:) (concatMap (nReachableRestricted' (toFigure v : l) (n-1))
                            (reachableRestricted g l v))

-- Why bother with StateT?
-- Morally nReachableRestrict is an almost pure function.
-- Given a graph, a list of visited figurate type, a path length, and a starting vertex v,
-- nReachableRestrict returns a list of all paths starting from v which contains
-- no two vertices of the same figurate type.
-- The only state the function carries around is the list of visited figurate type.
-- This state should be passed *explicitly* into the function.
-- State monad is useful when we have functions of the type
-- f :: State s b, i.e. modifies the state directly depending only on state.
-- In this case we can do:
-- do
--   b <- f
--   something b
-- It is not very useful if we only have functions of the form
-- a -> State s b
--
-- Also the point of functional programming is to have purity and
-- not having to keep track of state.
-- This allows us to skim code without mentally running them.
-- Using StateT is antithetical to this goal.
-- It pollutes everything with a mutable state.
--
-- reachableRestricted :: G.Graph -> [FigureType] -> G.Vertex -> [G.Vertex]
-- -- Returns a list of vertices directly reachable from v,
-- -- but only if the vertex is not one of the figurate types in l.
-- reachableRestricted g = reachableRestricted'
--   where
--     reachableRestricted' l v = filter ((`notElem` l) . toFigure) (edges v)
--       where edges = (g A.!)

type SearchState = (Path, [FigureType])

reachableRestrictedT :: ReaderT G.Graph (StateT SearchState []) Path
reachableRestrictedT = do
    g <- ask
    let edges :: G.Vertex -> [G.Vertex]
        edges = (g A.!)
        reachable :: StateT SearchState [] Path
        reachable = StateT go
          where
            go :: SearchState -> [(Path, SearchState)]
            go (p, s) = result
              where
                np :: [Path]
                np = do
                    v <- filter (\v -> toFigure v `notElem` s) (edges (head p))
                    return (v:p)
                result = map (\l -> (l, (l, toFigure (head l) : s))) np
    lift reachable

initState :: Int -> ReaderT G.Graph (StateT SearchState []) ()
initState n = (lift . StateT) $ const (map (\v -> ((), ([v], [n]))) (vertices n))

-- reachable :: StateT SearchState [] Path
-- reachable = StateT go
--   where
--     go :: SearchState -> [(Path, SearchState)]
--     go (p, s) = result
--       where
--         edges = (bestGraph A.!)
--         np = map (:) (edges (head p)) <*> p
--         result = map (\l -> (l, toFigure (head l) : s) np
        
   -- f :: StateT SearchState [] Path -> StateT SearchState [] Path
       -- SearchState -> [StateT searchState [] Path]
       -- SearchState -> [(Path, SearchState)]

-- newtype StateT s m a =
--   StateT { runStateT :: (s -> m (a,s)) }
--
-- instance (Monad m) => Monad (StateT s m) where
--   return a         = StateT $ \s -> return (a,s)
--   (StateT x) >>= f = StateT $ \s -> do
--     (v,s') <- x s          -- get new value and state
--     runStateT (f v) s'     -- pass them to f
--
--   return a         = StateT $ \s -> [(a,s)]
--   (StateT x) >>= f = StateT $ \s -> do
--   -- x :: s -> [(a, s)]
--     (v,s') <- x s          -- in List monad
--     runStateT (f v) s'     -- pass them to f
--
-- reachableRestrictedState :: StateT SearchState [] [G.Vertex]
-- reachableRestrictedState = do
--     (p, s) <- get
--     let edges 
    

-- -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- -- (>>=) :: ReaderT G.Graph (StateT [FigureType] []) [G.Vertex] -> ([G.Vertex] -> ReaderT G.Graph (StateT [FigureType] []) [G.Vertex]) -> ReaderT G.Graph (StateT [FigureType] []) [G.Vertex]
--
-- reachableT'
--   where
--     reachableT' :: G.Vertex -> StateT [FigureType] [] [G.Vertex]
--     reachableT' v = do
--         s <- get
--         put (v:s)
--         return (edges v)
--       where edges = (g A.!)

-- 
--
-- reachableStateT :: G.Graph -> [G.Vertex] -> StateT [Int] [] [G.Vertex]
-- reachableStateT g = reachableStateT'
--   where
--     edges = (g A.!)
--     reachableStateT' :: [G.Vertex] -> StateT [Int] [] [G.Vertex]
--     reachableStateT' l = StateT (\s -> map (\v -> (v:l, toFigure v:s)) (edges hv))
--         where hv = head l
-- 
-- helper :: [G.Vertex] -> [Int] -> [([G.Vertex], [Int])]
-- helper l s = map (\v -> (v:l, v:s)) (edges (head l))
--   where edges = (bestGraph A.!)
-- 
-- -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- -- (>>=) :: State [Int] [] [G.Vertex] -> ([G.Vertex] -> State s [] [G.Vertex]) -> State s [] [G.Vertex]
-- 
-- reachableStateTRestricted :: G.Graph -> [G.Vertex] -> StateT [Int] [] [G.Vertex]
-- reachableStateTRestricted g = reachableStateTRestricted'
--   where
--     edges = (g A.!)
--     reachableStateTRestricted' :: [G.Vertex] -> StateT [Int] [] [G.Vertex]
--     reachableStateTRestricted' l =
--         StateT (\s -> map (\v -> (v:l, toFigure v:s))
--                           (filter ((`notElem` s) . toFigure) (edges hv)))
--         where hv = head l
-- 
-- nReachableStateTRestriced :: G.Graph -> Int -> [G.Vertex] -> StateT [Int] [] [G.Vertex]
-- nReachableStateTRestriced g n = foldl1 (>=>) $ replicate n (reachableStateTRestricted g)

path :: [G.Vertex]
path = head $ filter (\x -> head x `elem` (bestGraph A.! last x))
                     (concatMap (nReachableRestricted bestGraph 5) (vertices 8))

main :: IO ()
main = print $ sum (map (`rem` figurePlace) path)










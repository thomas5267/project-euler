module Main where

import Data.Array qualified as A
import Data.Array.MArray qualified as MA
import Data.Array.ST qualified as STA

import Control.Monad.ST

type Vertex = Int

type Bounds = (Vertex,Vertex)

type Table a = A.Array Vertex a
type Graph = Table [Vertex]

data Tree a = Node a (Forest a)
type Forest a = [Tree a]


type Set s = STA.STArray s Vertex Bool

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = MA.newArray bnds False

contains :: Set s -> Vertex -> ST s Bool
contains m v = MA.readArray m v

include :: Set s -> Vertex -> ST s ()
include m v = MA.writeArray m v True

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST $ do
    m <- mkEmpty bnds
    chop m ts

chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop _ [] = return []
chop m (Node v ts : us) = do
    visited <- contains m v
    if visited
    then chop m us
    else do
        include m v
        as <- chop m ts
        bs <- chop m us
        return (Node v as : bs)

main :: IO ()
main = print "hello"

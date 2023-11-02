module Data.PairingHeap where

data PairingHeap a = Empty | PairNode a [PairingHeap a]

empty :: PairingHeap a
empty = Empty

singleton :: a -> PairingHeap a
singleton a = PairNode a []

union :: (Ord a) => PairingHeap a -> PairingHeap a -> PairingHeap a
union Empty heap = heap
union heap Empty = heap
union heap1@(PairNode x1 hl1) heap2@(PairNode x2 hl2)
  | x1 <= x2  = PairNode x1 (heap2 : hl1)
  | otherwise = PairNode x2 (heap1 : hl2)

insert :: (Ord a) => a -> PairingHeap a -> PairingHeap a
insert x = union (singleton x)


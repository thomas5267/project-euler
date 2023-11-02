module Sudoku (

  Indexable(..),

  Sudoku(..),
  SudokuChoice, sudoku, simap, sudokuIndices,

  SudokuShaped(..),

  SudokuSlice(..),
  row, col, box,
  byRow, byCol, byBox,

  test

) where

import Euler.Helper

import Data.Int

import Data.Ix qualified as Ix

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U


class (Ix.Ix (Index s)) => Indexable s where
  type Index s
  type Value s
  bounds :: s -> (Index s, Index s)
  (!) :: s -> Index s -> Value s
  update :: s -> [(Index s, Value s)] -> s


newtype Sudoku = Sudoku { unSudoku :: U.Vector Int8 }
    deriving (Show, Eq)
-- Type Sudoku is reserved for a 9x9 grid.
-- I am sure the size of the array can be statically typed
-- but I am not sure whether it is worth my time to do so.

-- xIndexer is a function transforming an index of x to
-- an index of the underlying vector representation.
sudokuIndexer :: Index Sudoku -> Int
sudokuIndexer (r, c) = 9*r + c
{-# INLINE sudokuIndexer #-}

instance Indexable Sudoku where
  type Index Sudoku = (Int, Int)
  type Value Sudoku = Int8
  bounds = const ((0, 0), (8, 8))
  s ! (r, c) = unSudoku s U.! (9*r + c)
  update s xs = Sudoku $ unSudoku s U.// map (mapFst sudokuIndexer) xs

type SudokuChoice = (Index Sudoku, Value Sudoku)

sudoku :: [Int8] -> Sudoku
sudoku xs = Sudoku $ U.fromList xs

sudokuIndices :: V.Vector (Int, Int)
sudokuIndices = V.fromList [ (x, y) | x <- [0..8], y <- [0..8] ]

simap :: (Index Sudoku -> Int8 -> a) -> Sudoku -> SudokuShaped a
simap f = SudokuShaped . V.zipWith f sudokuIndices . U.convert . unSudoku


newtype SudokuShaped a = SudokuShaped { unSudokuShaped :: V.Vector a }
    deriving (Show, Eq)

instance Indexable (SudokuShaped a) where
  type Index (SudokuShaped a) = (Int, Int)
  type Value (SudokuShaped a) = a
  bounds = const ((0, 0), (8, 8))
  s ! (r, c) = unSudokuShaped s V.! (9*r + c)
  update s xs = SudokuShaped $ unSudokuShaped s V.// map (mapFst sudokuIndexer) xs


newtype SudokuSlice = SudokuSlice { unSudokuSlice :: U.Vector Int8 }
    deriving (Show, Eq)

row :: Sudoku -> Int -> SudokuSlice
row s r = SudokuSlice $ U.slice (9*r) 9 (unSudoku s)

byRow :: Index Sudoku -> Index Sudoku
byRow = id
{-# INLINE byRow #-}

col :: Sudoku -> Int -> SudokuSlice
col s c = SudokuSlice $ U.backpermute (unSudoku s) (U.enumFromStepN c 9 9)

byCol :: Index Sudoku -> Index Sudoku
byCol (c, r) = (r, c)
{-# INLINE byCol #-}

box :: Sudoku -> Int -> SudokuSlice
box s b = SudokuSlice $ U.backpermute (unSudoku s) indexVec
  where
    indexVec = U.generate 9 (sudokuIndexer . byBox . (b,))

byBox :: Index Sudoku -> Index Sudoku
byBox (b, i)
  | i <= 2    = (br, bc+i)
  | i <= 5    = (br+1, bc+i-3)
  | otherwise = (br+2, bc+i-6)
  where
    -- (br, bc) is the index for top-left corner of the box.
    (m, n) = b `quotRem` 3
    (br, bc) = (3*m, 3*n)
{-# INLINE byBox #-}

test :: Sudoku
test = sudoku
    [ 0, 0, 3,  0, 2, 0,  6, 0, 0
    , 9, 0, 0,  3, 0, 5,  0, 0, 1
    , 0, 0, 1,  8, 0, 6,  4, 0, 0

    , 0, 0, 8,  1, 0, 2,  9, 0, 0
    , 7, 0, 0,  0, 0, 0,  0, 0, 8
    , 0, 0, 6,  7, 0, 8,  2, 0, 0

    , 0, 0, 2,  6, 0, 9,  5, 0, 0
    , 8, 0, 0,  2, 0, 3,  0, 0, 9
    , 0, 0, 5,  0, 1, 0,  3, 0, 0
    ]


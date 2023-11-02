module Main (main) where

import Euler.Helper

import Data.Bifunctor
import Data.Char (digitToInt)
import Data.Foldable
import Data.Function
import Data.List (groupBy, sortBy)
import Data.List.Split
import Data.Maybe
import Data.Ord

import Data.Bit.ThreadSafe
import Data.Bits
import Data.Int
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

import Control.Applicative
import Control.Monad
import Control.Monad.Except

-- import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

newtype Row = Row (Int, Int)
  deriving (Show, Eq, Ord)

newtype Col = Col (Int, Int)
  deriving (Show, Eq, Ord)

newtype Box = Box (Int, Int)
  deriving (Show, Eq, Ord)

class SudokuIndex i where
  sudokuIndexer :: i -> Int

  -- toRow and fromRow should form an isomorphism, at least within the range
  -- Row (0, 0) to Row (8, 8)
  -- For all SudokuIndex i, the following should hold true.
  -- (toRow :: i -> Row) . (fromRow :: Row -> i) = id
  -- fromRow . toRow = id :: i -> i
  fromRow :: Row -> i
  toRow :: i -> Row

instance SudokuIndex Row where
  sudokuIndexer (Row (r, c)) = 9 * r + c
  fromRow = id
  toRow = id
  {-# INLINE sudokuIndexer #-}
  {-# INLINE [1] fromRow #-}
  {-# INLINE [1] toRow #-}

instance SudokuIndex Col where
  sudokuIndexer (Col (c, r)) = 9 * r + c
  fromRow (Row (r, c)) = Col (c, r)
  toRow (Col (c, r)) = Row (r, c)
  {-# INLINE sudokuIndexer #-}
  {-# INLINE [1] fromRow #-}
  {-# INLINE [1] toRow #-}

instance SudokuIndex Box where
  sudokuIndexer = sudokuIndexer . toRow
  fromRow = rowToBox
  toRow = boxToRow
  {-# INLINE sudokuIndexer #-}

boxToRow :: Box -> Row
boxToRow (Box (b, i))
  | i < 3 = Row (br, bc + i)
  | i < 6 = Row (br + 1, bc + i - 3)
  | otherwise = Row (br + 2, bc + i - 6)
  where
    -- (br, bc) is the index for top-left corner of the box.
    (m, n) = b `quotRem` 3
    (br, bc) = (3 * m, 3 * n)
{-# INLINE boxToRow #-}

rowToBox :: Row -> Box
rowToBox (Row (r, c)) = Box (3 * m + p, 3 * n + q)
  where
    (m, n) = r `quotRem` 3
    (p, q) = c `quotRem` 3
{-# INLINE rowToBox #-}

convertIndex :: (SudokuIndex i, SudokuIndex j) => i -> j
convertIndex = fromRow . toRow
{-# INLINE [1] convertIndex #-}

{-# RULES
"convertIndex/i" convertIndex = id
  #-}

rowIndex :: (SudokuIndex i) => i -> Int
rowIndex = (\(Row (r, _)) -> r) . convertIndex
{-# INLINE rowIndex #-}

colIndex :: (SudokuIndex i) => i -> Int
colIndex = (\(Col (c, _)) -> c) . convertIndex
{-# INLINE colIndex #-}

boxIndex :: (SudokuIndex i) => i -> Int
boxIndex = (\(Box (b, _)) -> b) . convertIndex
{-# INLINE boxIndex #-}

type SudokuChoice i = (i, Value Sudoku)

class SudokuShaped s where
  type Value s
  (!) :: (SudokuIndex i) => s -> i -> Value s
  update :: (SudokuIndex i) => s -> SudokuChoice i -> s
  updates :: (SudokuIndex i) => s -> [SudokuChoice i] -> s

class (SudokuShaped s) => SudokuPrim s where
  undo :: (SudokuIndex i) => s -> SudokuChoice i -> s
  undos :: (SudokuIndex i) => s -> [SudokuChoice i] -> s

-- Sudoku is reserved for a 9x9 grid.
-- I am sure the size of the array can be statically typed
-- but I am not sure whether it is worth my time to do so.
newtype Sudoku = Sudoku
  { unSudoku :: U.Vector (Value Sudoku)
  }
  deriving (Show, Eq)

instance SudokuShaped Sudoku where
  type Value Sudoku = Int8
  (!) s i = unSudoku s U.! sudokuIndexer i
  update s (i, c) = Sudoku $ U.modify (\mv -> MU.write mv (sudokuIndexer i) c) (unSudoku s)
  updates s xs = Sudoku $ unSudoku s U.// map (first sudokuIndexer) xs

instance SudokuPrim Sudoku where
  undo s (i, _) = Sudoku $ U.modify (\mv -> MU.write mv (sudokuIndexer i) 0) (unSudoku s)
  undos s xs = Sudoku $ unSudoku s U.// map ((,0) . sudokuIndexer . fst) xs

sudoku :: [Value Sudoku] -> Sudoku
sudoku xs = Sudoku $ U.fromList xs

rowMajorTraversal :: (SudokuIndex i) => [i]
rowMajorTraversal = [convertIndex $ Row (x, y) | x <- [0 .. 8], y <- [0 .. 8]]

newtype SudokuSlice = SudokuSlice
  { unSudokuSlice :: U.Vector (Value Sudoku)
  }
  deriving (Show, Eq)

row :: Sudoku -> Int -> SudokuSlice
row s r = SudokuSlice $ U.slice (9 * r) 9 (unSudoku s)

col :: Sudoku -> Int -> SudokuSlice
col s c = SudokuSlice $ U.backpermute (unSudoku s) (U.enumFromStepN c 9 9)

box :: Sudoku -> Int -> SudokuSlice
box s b = SudokuSlice $ U.backpermute (unSudoku s) indexVec
  where
    indexVec = U.generate 9 (sudokuIndexer . Box . (b,))

-- test :: Sudoku
-- test = sudoku
--     [ 0, 0, 3,  0, 2, 0,  6, 0, 0
--     , 9, 0, 0,  3, 0, 5,  0, 0, 1
--     , 0, 0, 1,  8, 0, 6,  4, 0, 0
--
--     , 0, 0, 8,  1, 0, 2,  9, 0, 0
--     , 7, 0, 0,  0, 0, 0,  0, 0, 8
--     , 0, 0, 6,  7, 0, 8,  2, 0, 0
--
--     , 0, 0, 2,  6, 0, 9,  5, 0, 0
--     , 8, 0, 0,  2, 0, 3,  0, 0, 9
--     , 0, 0, 5,  0, 1, 0,  3, 0, 0
--     ]

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

newtype SudokuSet = SudokuSet {unSudokuSet :: U.Vector Bit}
  deriving (Show, Eq)

intersect :: SudokuSet -> SudokuSet -> SudokuSet
intersect (SudokuSet a) (SudokuSet b) = SudokuSet $ zipBits (.&.) a b
{-# INLINE intersect #-}

instance Semigroup SudokuSet where
  (<>) = intersect

instance Monoid SudokuSet where
  mempty = SudokuSet $ U.replicate 9 (Bit True)

-- SudokuXPage represents the possible values for a cell in any row, columns or boxes.
-- Any Sudoku puzzle has three SudokuPage, one for the columns, one for the rows, and one
-- for the boxes.
-- To calculate the possible values a cell can take, query the three SudokuXPages and
-- intersect them.
-- The pages are stored as flat vectors in a way that is best explained in an example.
-- RowPage $ U.fromList [r1b1, r1b2, r1b3..], where rxbd is a boolean
-- indicating if d is still available in row x.
-- It is stored this way to allow for O(1) no-copy slicing instead of O(n)
-- backpermute.

newtype SudokuRowPage = SudokuRowPage {unSudokuRowPage :: U.Vector Bit}
  deriving (Show, Eq)

sudokuRowPage :: Sudoku -> SudokuRowPage
sudokuRowPage s = SudokuRowPage (U.replicate 81 (Bit True)) `updates` us
  where
    us = filter ((/= 0) . snd) $ zip @Row rowMajorTraversal (U.toList $ unSudoku s)

instance SudokuShaped SudokuRowPage where
  type Value SudokuRowPage = SudokuSet
  (!) p i = SudokuSet $ U.slice (9 * rowIndex i) 9 $ unSudokuRowPage p
  update p (i, c) = SudokuRowPage $ U.modify (\mv -> MU.write mv j (Bit False)) (unSudokuRowPage p)
    where
      j = (9 * rowIndex i) + fromIntegral c - 1
  updates p xs = SudokuRowPage $ unSudokuRowPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * rowIndex i + fromIntegral c - 1, Bit False)) xs

instance SudokuPrim SudokuRowPage where
  undo p (i, c) = SudokuRowPage $ U.modify (\mv -> MU.write mv j (Bit True)) (unSudokuRowPage p)
    where
      j = (9 * rowIndex i) + fromIntegral c - 1
  undos p xs = SudokuRowPage $ unSudokuRowPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * rowIndex i + fromIntegral c - 1, Bit True)) xs

newtype SudokuColPage = SudokuColPage {unSudokuColPage :: U.Vector Bit}
  deriving (Show, Eq)

sudokuColPage :: Sudoku -> SudokuColPage
sudokuColPage s = SudokuColPage (U.replicate 81 (Bit True)) `updates` us
  where
    us = filter ((/= 0) . snd) $ zip @Row rowMajorTraversal (U.toList $ unSudoku s)

instance SudokuShaped SudokuColPage where
  type Value SudokuColPage = SudokuSet
  (!) p i = SudokuSet $ U.slice (9 * colIndex i) 9 $ unSudokuColPage p
  update p (i, c) = SudokuColPage $ U.modify (\mv -> MU.write mv j (Bit False)) (unSudokuColPage p)
    where
      j = (9 * colIndex i) + fromIntegral c - 1
  updates p xs = SudokuColPage $ unSudokuColPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * colIndex i + fromIntegral c - 1, Bit False)) xs

instance SudokuPrim SudokuColPage where
  undo p (i, c) = SudokuColPage $ U.modify (\mv -> MU.write mv j (Bit True)) (unSudokuColPage p)
    where
      j = (9 * colIndex i) + fromIntegral c - 1
  undos p xs = SudokuColPage $ unSudokuColPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * colIndex i + fromIntegral c - 1, Bit True)) xs

newtype SudokuBoxPage = SudokuBoxPage {unSudokuBoxPage :: U.Vector Bit}
  deriving (Show, Eq)

sudokuBoxPage :: Sudoku -> SudokuBoxPage
sudokuBoxPage s = SudokuBoxPage (U.replicate 81 (Bit True)) `updates` us
  where
    us = filter ((/= 0) . snd) $ zip @Row rowMajorTraversal (U.toList $ unSudoku s)

instance SudokuShaped SudokuBoxPage where
  type Value SudokuBoxPage = SudokuSet
  (!) p i = SudokuSet $ U.slice (9 * boxIndex i) 9 $ unSudokuBoxPage p
  update p (i, c) = SudokuBoxPage $ U.modify (\mv -> MU.write mv j (Bit False)) (unSudokuBoxPage p)
    where
      j = (9 * boxIndex i) + fromIntegral c - 1
  updates p xs = SudokuBoxPage $ unSudokuBoxPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * boxIndex i + fromIntegral c - 1, Bit False)) xs

instance SudokuPrim SudokuBoxPage where
  undo p (i, c) = SudokuBoxPage $ U.modify (\mv -> MU.write mv j (Bit True)) (unSudokuBoxPage p)
    where
      j = (9 * boxIndex i) + fromIntegral c - 1
  undos p xs = SudokuBoxPage $ unSudokuBoxPage p U.// us
    where
      us :: [(Int, Bit)]
      us = map (\(i, c) -> (9 * boxIndex i + fromIntegral c - 1, Bit True)) xs

data SudokuPage = SudokuPage
  { rowPage :: {-# UNPACK #-} !SudokuRowPage
  , colPage :: {-# UNPACK #-} !SudokuColPage
  , boxPage :: {-# UNPACK #-} !SudokuBoxPage
  }
  deriving (Show, Eq)

sudokuPage :: Sudoku -> SudokuPage
sudokuPage s =
  SudokuPage
    { rowPage = sudokuRowPage s
    , colPage = sudokuColPage s
    , boxPage = sudokuBoxPage s
    }

instance SudokuShaped SudokuPage where
  type Value SudokuPage = SudokuSet
  (!) p i = rowPage p ! i <> colPage p ! i <> boxPage p ! i
  update s x =
    SudokuPage
      { rowPage = update (rowPage s) x
      , colPage = update (colPage s) x
      , boxPage = update (boxPage s) x
      }
  updates s xs =
    SudokuPage
      { rowPage = updates (rowPage s) xs
      , colPage = updates (colPage s) xs
      , boxPage = updates (boxPage s) xs
      }

instance SudokuPrim SudokuPage where
  undo s x =
    SudokuPage
      { rowPage = undo (rowPage s) x
      , colPage = undo (colPage s) x
      , boxPage = undo (boxPage s) x
      }
  undos s xs =
    SudokuPage
      { rowPage = undos (rowPage s) xs
      , colPage = undos (colPage s) xs
      , boxPage = undos (boxPage s) xs
      }

-- data History i
--   = Guess (SudokuChoice i)
--   | Deduce [SudokuChoice i]
--   deriving (Show, Eq)

data SudokuState i = SudokuState
  { board :: {-# UNPACK #-} !Sudoku
  , page :: {-# UNPACK #-} !SudokuPage
  , history :: [[SudokuChoice i]]
  }
  deriving (Show, Eq)

sudokuState :: Sudoku -> SudokuState i
sudokuState s =
  SudokuState
    { board = s
    , page = sudokuPage s
    , history = []
    }

safeTail :: [x] -> [x]
safeTail [] = []
safeTail (_ : xs) = xs

instance (SudokuIndex i) => SudokuShaped (SudokuState i) where
  type Value (SudokuState i) = (Int8, SudokuSet)
  (!) s i = (board s ! i, page s ! i)
  update s x =
    SudokuState
      { board = update (board s) x
      , page = update (page s) x
      , history = [first convertIndex x] : history s
      }
  updates s xs =
    SudokuState
      { board = updates (board s) xs
      , page = updates (page s) xs
      , history = case history s of
          [] -> [map (first convertIndex) xs]
          (h : hs) -> (map (first convertIndex) xs ++ h) : hs
      }

type SudokuProcessor i r = MaybeT (State (SudokuState i)) r

-- A SudokuState is inconsistent if there is an unfilled cell which cannot be filled.
-- How can a SudokuState become inconsistent and how the inconsistency is discovered?
-- 1. A guess is made incorrectly
-- Then one of the following
-- 2a. An unfilled cell which has no candidates.
--     Discovered when (traverse (availableNum s) rowMajorTraversal) is called.
-- 2b. The same number is assigned to two cells in the same unit in deduce.
--     Discovered by the (isConsistent us) check.

-- availableNum :: (SudokuIndex i) => SudokuState -> i -> Maybe (i, [Value Sudoku])
-- availableNum s i
--   | n == 0 = case cs of
--                 [] -> Nothing
--                 _  -> pure (i, cs)
--   | otherwise = pure (i, [])
--   where
--     (n, set) = s ! i
--     cs = map ((+1) . fromIntegral) . listBits . unSudokuSet $ set

availableNum :: (SudokuIndex i) => i -> SudokuProcessor i (i, [Value Sudoku])
availableNum i = do
  (n, set) <- gets (! i)
  if n == 0
    then
      let cs = map ((+ 1) . fromIntegral) . listBits . unSudokuSet $ set
       in case cs of
            [] -> empty
            _ -> pure (i, cs)
    else pure (i, [])

checkCompleted :: SudokuState i -> Bool
checkCompleted !s = U.notElem 0 . unSudoku . board $ s

-- deduce :: SudokuState -> Maybe SudokuState
-- deduce !s = us0 >>= core
--   where
--     core us
--       | null us = pure s
--       | isConsistent us = deduce $ s `updates` us
--       | otherwise = Nothing
--
--     us0 :: Maybe [SudokuChoice Row]
--     us0 = sortBy (comparing snd)
--      . mapMaybe (traverse onlyOneChoice)
--      <$> traverse (availableNum s) rowMajorTraversal
--
--
--     onlyOneChoice :: [a] -> Maybe a
--     onlyOneChoice [x] = Just x
--     onlyOneChoice _   = Nothing
--     -- isConsistent v returns Nothing if the update list is consistent,
--     -- and Just a if it is not, where a is the address of an inconsistency.
--     -- A list is inconsistent if it assigns the same number to two cells of the
--     -- same row, column or box.
--     -- The update list needs to be checked because we have made guesses instead
--     -- of pure logical deduction.
--     -- If one of the guesses is wrong, deduce could possibly generate an update
--     -- list which assigns the same number to two cells in the same unit.
--     -- If the board is consistent but partially fill, then this check always
--     -- passes.
--     -- However, if we have guessed incorrectly then this check would be necessary
--     -- to prevent incorrect deductions.
--
--     isConsistent :: (SudokuIndex i) => [(i, Int8)] -> Bool
--     isConsistent us = isConsistentRow uss0
--         && isConsistentCol uss0
--         && isConsistentBox uss0
--       where
--         uss0 = groupBy ((==) `on` snd) us
--
--         isConsistentRow :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
--         isConsistentRow uss = all go uss
--           where
--             go [] = True
--             go [_] = True
--             go ((ix, _):xs) = all (\ (iy, _) -> rowIndex ix /= rowIndex iy) xs
--
--         isConsistentCol :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
--         isConsistentCol uss = all go uss
--           where
--             go [] = True
--             go [_] = True
--             go ((ix, _):xs) = all (\ (iy, _) -> colIndex ix /= colIndex iy) xs
--
--         isConsistentBox :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
--         isConsistentBox uss = all go uss
--           where
--             go [] = True
--             go [_] = True
--             go ((ix, _):xs) = all (\ (iy, _) -> boxIndex ix /= boxIndex iy) xs

deduce :: (SudokuIndex i) => SudokuProcessor i ()
deduce = do
  --     us0 <- sortBy (comparing snd)
  --         . mapMaybe (traverse onlyOneChoice)
  --         <$> fmap sequenceA
  --         $ traverse availableNum rowMajorTraversal
  us0 <-
    sortBy (comparing snd)
      . mapMaybe (traverse onlyOneChoice)
      <$> traverse availableNum rowMajorTraversal
  core us0
  where
    -- availableNum :: (SudokuIndex i) => i -> SudokuProcessor (i, [Value Sudoku])
    -- traverse :: (SudokuIndex i) => (i -> SudokuProcessor (i, [Value Sudoku]))
    --                             -> [i]
    --                             -> SudokuProcessor [(i, [Value Sudoku])]

    core :: (SudokuIndex i) => [SudokuChoice i] -> SudokuProcessor i ()
    core !us
      | null us = pure ()
      | isConsistent us = modify' (`updates` us) >> deduce
      | otherwise = empty

    --     us0 :: Maybe [SudokuChoice Row]
    --     us0 = sortBy (comparing snd)
    --      . mapMaybe (traverse onlyOneChoice)
    --      <$> traverse (availableNum s) rowMajorTraversal

    onlyOneChoice :: [a] -> Maybe a
    onlyOneChoice [a] = Just a
    onlyOneChoice _ = Nothing
    -- isConsistent v returns Nothing if the update list is consistent,
    -- and Just a if it is not, where a is the address of an inconsistency.
    -- A list is inconsistent if it assigns the same number to two cells of the
    -- same row, column or box.
    -- The update list needs to be checked because we have made guesses instead
    -- of pure logical deduction.
    -- If one of the guesses is wrong, deduce could possibly generate an update
    -- list which assigns the same number to two cells in the same unit.
    -- If the board is consistent but partially fill, then this check always
    -- passes.
    -- However, if we have guessed incorrectly then this check would be necessary
    -- to prevent incorrect deductions.

    isConsistent :: (SudokuIndex i) => [(i, Int8)] -> Bool
    isConsistent us =
      isConsistentRow us'
        && isConsistentCol us'
        && isConsistentBox us'
      where
        us' = groupBy ((==) `on` snd) us

        isConsistentRow :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
        isConsistentRow = all go
          where
            go [] = True
            go [_] = True
            go ((ix, _) : xs) = all (\(iy, _) -> rowIndex ix /= rowIndex iy) xs

        isConsistentCol :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
        isConsistentCol = all go
          where
            go [] = True
            go [_] = True
            go ((ix, _) : xs) = all (\(iy, _) -> colIndex ix /= colIndex iy) xs

        isConsistentBox :: (SudokuIndex i) => [[(i, Int8)]] -> Bool
        isConsistentBox = all go
          where
            go [] = True
            go [_] = True
            go ((ix, _) : xs) = all (\(iy, _) -> boxIndex ix /= boxIndex iy) xs

-- A good cell is a cell that has a minimum amount of choices.
-- After deduceAll, all cells must have at least two choices.
-- If we find a cell with exactly two choices, we can exit early.
-- findGoodCell makes more comparison than necessary.
-- It early exits when a cell with two choices is encountered but still
-- makes unnecessary comparisons with elements that come before it.
findGoodCell :: (SudokuIndex i) => SudokuProcessor i (i, [Value Sudoku])
findGoodCell =
  foldr1 go
    . filter (not . null . snd)
    <$> traverse availableNum rowMajorTraversal
  where
    isGood [_, _] = True
    isGood _ = False

    go :: (i, [Value Sudoku]) -> (i, [Value Sudoku]) -> (i, [Value Sudoku])
    go x@(_, ds1) acc@(_, ds2) =
      if isGood ds1
        then x
        else case compareLength ds1 ds2 of
          LT -> x
          _ -> acc

catchNothing :: (Monad m) => MaybeT m a -> MaybeT m a -> MaybeT m a
catchNothing m handler = MaybeT $ do
  a <- runMaybeT m
  case a of
    Nothing -> runMaybeT handler
    Just x -> pure $ Just x

rollback :: (SudokuIndex i) => SudokuProcessor i ()
rollback = modify' go
  where
    go s = case history s of
      [] -> s
      (h : hs) ->
        SudokuState
          { board = undos (board s) h
          , page = undos (page s) h
          , history = hs
          }

fill :: (SudokuIndex i) => SudokuProcessor i ()
fill = deduce >> core
  where
    core :: (SudokuIndex i) => SudokuProcessor i ()
    core = do
      completed <- gets checkCompleted
      if completed
        then pure ()
        else do
          (i, cs) <- findGoodCell
          foldr (go i) empty cs
    --       | checkCompleted s = pure s
    --       | otherwise = asum . map core . nextStates $ s

    go :: (SudokuIndex i) => i -> Value Sudoku -> SudokuProcessor i () -> SudokuProcessor i ()
    go i c acc = (guess >> deduce >> core) `catchNothing` (rollback >> acc)
      where
        guess = modify' (`update` (i, c))

-- groupsOf :: Int -> [a] -> [[a]]
-- groupsOf _ [] = []
-- groupsOf !n xs = as : groupsOf n bs
--     where
--       (as, bs) = splitAt n xs

-- dropEvery :: Int -> [x] -> [x]
-- dropEvery n0 xs0 = dropEvery' n0 xs0 []
--     where
--       dropEvery' _ [] = id
--       dropEvery' n xs = (take (n-1) xs ++) . dropEvery' n (drop n xs)

trimFile :: [String] -> [String]
trimFile = dropEvery 10 . tail
  where
    dropEvery :: Int -> [x] -> [x]
    dropEvery n0 xs0 = dropEvery' n0 xs0 []
      where
        dropEvery' _ [] = id
        dropEvery' n xs = (take (n - 1) xs ++) . dropEvery' n (drop n xs)

--     dropEvery _ [] = []
--     dropEvery n (_:xs) = as ++ dropEvery n bs
--       where
--         (as, bs) = splitAt (n-1) xs

parseSudoku :: String -> SudokuState i
parseSudoku = sudokuState . sudoku . map (fromIntegral . digitToInt)

-- extractor :: Maybe (SudokuState i) -> Int
-- extractor (Just s) = toInt . U.slice 0 3 . unSudoku . board $ s
--   where
--     toInt :: U.Vector Int8 -> Int
--     toInt = U.foldl' (\ acc d -> acc*10 + fromIntegral d) 0
-- extractor _ = errorWithoutStackTrace "extractor: failed to extract"

extractor :: (Maybe (), SudokuState i) -> Int
extractor (Just (), s) = toInt . U.slice 0 3 . unSudoku . board $ s
  where
    toInt :: U.Vector Int8 -> Int
    toInt = U.foldl' (\acc d -> acc * 10 + fromIntegral d) 0
extractor _ = errorWithoutStackTrace "extractor: failed to extract"

readSudoku :: IO [SudokuState i]
readSudoku =
  map parseSudoku
    . divvy 81 88
    . filter (/= '\n')
    . drop 8
    <$> readFile "p096_sudoku.txt"

main :: IO ()
-- main = print "hello"
main = do
  sudokuList <- readSudoku
  print $ foldl' (+) 0 . map (extractor . (runState . runMaybeT $ fill @Row)) $ sudokuList

module Main (main) where

import Euler.Helper

import Sudoku

import Data.Int
import Data.Char (digitToInt)

import Data.List (foldl1')

import Data.Ix as Ix

import Data.Vector qualified as V
import Data.Vector.Algorithms.Insertion qualified as V.IS

import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Bits
import Data.Bit.ThreadSafe

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Except


newtype SudokuSet = SudokuSet { unSudokuSet :: U.Vector Bit }
    deriving (Show, Eq)

sudokuSetIndexer :: Int -> Int
sudokuSetIndexer d = d-1
{-# INLINE sudokuSetIndexer #-}

instance Indexable SudokuSet where
  type Index SudokuSet = Int
  type Value SudokuSet = Bool
  bounds = const (1, 9)
  s ! d = unBit $ unSudokuSet s U.! sudokuSetIndexer d
  update s xs = SudokuSet $ U.modify go (unSudokuSet s)
    where
      go :: forall s. MVector s Bit -> ST s ()
      go mv = forM_ xs (\ (d, b) -> do
          let i = sudokuSetIndexer d
          a <- MU.read mv i
          when (unBit a) $ MU.write mv i (Bit b)
                       )

intersect :: SudokuSet -> SudokuSet -> SudokuSet
intersect (SudokuSet a) (SudokuSet b) = SudokuSet $ zipBits (.&.) a b

sudokuSetToList :: SudokuSet -> [Int8]
sudokuSetToList = map ((+1) . fromIntegral) . listBits . unSudokuSet

instance Semigroup SudokuSet where
  (<>) = intersect

instance Monoid SudokuSet where
  mempty = SudokuSet $ U.replicate 9 (Bit True)


-- Represents the possible values for a cell in any boxes, any rows, or any columns.
-- Any Sudoku puzzle has three SudokuPage,
-- one for the columns, one for the rows, and one for the boxes.
-- To calculate the possible values a cell can take,
-- query the three SudokuSetPage and intersect them.
-- The pages are stored as flat vectors in a way that is best explained in an example.
-- RowPage $ U.fromList [r1b1, r1b2, r1b3..]
-- where rxbd is a boolean indicating if d is still available in row x.
-- It is stored this way to allow for O(1) no-copy slicing
-- instead of O(n) backpermute.

class SudokuPage p where
  query :: p -> Index Sudoku -> SudokuSet
  updateChoices :: p -> [SudokuChoice] -> p


newtype SudokuRowPage = SudokuRowPage { unSudokuRowPage :: U.Vector Bit }
    deriving (Show, Eq)

rowPageIndexer :: Index SudokuRowPage -> Int
rowPageIndexer (r, d) = 9*r + d-1
{-# INLINE rowPageIndexer #-}

instance Indexable SudokuRowPage where
  type Index SudokuRowPage = (Int, Int)
  type Value SudokuRowPage = Bool
  bounds = const ((0, 1), (8, 9))
  p ! a = unBit $ unSudokuRowPage p U.! rowPageIndexer a
  update p xs = SudokuRowPage $ U.modify go (unSudokuRowPage p)
    where
      go :: forall s. MVector s Bit -> ST s ()
      go mv = forM_ xs (\ (a, b) -> do
          let i = rowPageIndexer a
          cur <- MU.read mv i
          when (unBit cur) $ MU.write mv i (Bit b)
                       )

toRowChoice :: SudokuChoice -> (Index SudokuRowPage, Value SudokuRowPage)
toRowChoice ((r, _), d) = ((r, fromIntegral d), False)
{-# INLINE toRowChoice #-}

instance SudokuPage SudokuRowPage where
  query (SudokuRowPage p) (r, _) = SudokuSet $ U.slice (9*r) 9 p
  updateChoices p xs = update p $ map toRowChoice xs

sudokuRowPage :: Sudoku -> SudokuRowPage
sudokuRowPage s = updateChoices emptyRowPage updateList
  where
    emptyRowPage = SudokuRowPage $ U.replicate 81 (Bit True)
    rowIndices = Ix.range $ bounds s
    updateList = filter ((/= 0) . snd) $ map (toSnd (s !)) rowIndices


newtype SudokuColPage = SudokuColPage { unSudokuColPage :: U.Vector Bit }
    deriving (Show, Eq)

colPageIndexer :: Index SudokuColPage -> Int
colPageIndexer (c, d) = 9*c + d-1
{-# INLINE colPageIndexer #-}

instance Indexable SudokuColPage where
  type Index SudokuColPage = (Int, Int)
  type Value SudokuColPage = Bool
  bounds = const ((0, 1), (8, 9))
  p ! a = unBit $ unSudokuColPage p U.! colPageIndexer a
  update p xs = SudokuColPage $ U.modify go $ unSudokuColPage p
    where
      go :: forall s. MVector s Bit -> ST s ()
      go mv = forM_ xs (\ (a, b) -> do
          let i = colPageIndexer a
          cur <- MU.read mv i
          when (unBit cur) $ MU.write mv i (Bit b)
                       )

toColChoice :: SudokuChoice -> (Index SudokuColPage, Value SudokuColPage)
toColChoice ((_, c), d) = ((c, fromIntegral d), False)
{-# INLINE toColChoice #-}

instance SudokuPage SudokuColPage where
  query (SudokuColPage p) (_, c) = SudokuSet $ U.slice (9*c) 9 p
  updateChoices p xs = update p $ map toColChoice xs

sudokuColPage :: Sudoku -> SudokuColPage
sudokuColPage s = updateChoices emptyColPage updateList
  where
    emptyColPage = SudokuColPage $ U.replicate 81 (Bit True)
    colIndices = map byCol $ Ix.range (bounds s)
    updateList = filter ((/= 0) . snd) $ map (toSnd (s !)) colIndices


newtype SudokuBoxPage = SudokuBoxPage { unSudokuBoxPage :: U.Vector Bit } deriving (Show, Eq)

boxPageIndexer :: Index SudokuBoxPage -> Int
boxPageIndexer (b, d) = 9*b + d-1
{-# INLINE boxPageIndexer #-}

instance Indexable SudokuBoxPage where
  type Index SudokuBoxPage = (Int, Int)
  type Value SudokuBoxPage = Bool
  bounds = const ((0, 1), (8, 9))
  p ! a = unBit $ unSudokuBoxPage p U.! boxPageIndexer a
  update p xs = SudokuBoxPage $ U.modify go (unSudokuBoxPage p)
    where
      go :: forall s. MVector s Bit -> ST s ()
      go mv = forM_ xs (\ (a, b) -> do
          let i = boxPageIndexer a
          cur <- MU.read mv i
          when (unBit cur) $ MU.write mv i (Bit b)
                       )

toBoxChoice :: SudokuChoice -> (Index SudokuColPage, Value SudokuColPage)
toBoxChoice (a, d) = ((toBox a, fromIntegral d), False)
  where
    toBox (r, c) = 3*m + n
      where
        m = r `quot` 3
        n = c `quot` 3
{-# INLINE toBoxChoice #-}

instance SudokuPage SudokuBoxPage where
  query (SudokuBoxPage p) (r, c) = SudokuSet $ U.slice (9*b) 9 p
    where
      b = 3*(r `quot` 3) + c `quot` 3
  updateChoices p xs = update p $ map toBoxChoice xs


sudokuBoxPage :: Sudoku -> SudokuBoxPage
sudokuBoxPage s = updateChoices emptyBoxPage updateList
  where
    emptyBoxPage = SudokuBoxPage $ U.replicate 81 (Bit True)
    boxIndices = map byBox $ Ix.range (bounds s)
    updateList = filter ((/= 0) . snd) $ map (toSnd (s !)) boxIndices


data SudokuState = SudokuState
    { board :: Sudoku
    , rowPage :: SudokuRowPage
    , colPage :: SudokuColPage
    , boxPage :: SudokuBoxPage
    } deriving (Show, Eq)

sudokuState :: Sudoku -> SudokuState
sudokuState s = SudokuState
    { board = s
    , rowPage = sudokuRowPage s
    , colPage = sudokuColPage s
    , boxPage = sudokuBoxPage s
    }

instance SudokuPage SudokuState where
  query s p = if board s ! p == 0
              then foldl1' intersect
                    [ rowPage s `query` p, colPage s `query` p, boxPage s `query` p ]
              else mempty
  updateChoices s xs = s { board = update (board s) xs
                         , rowPage = updateChoices (rowPage s) xs
                         , colPage = updateChoices (colPage s) xs
                         , boxPage = updateChoices (boxPage s) xs
                         }


data SudokuError = NoChoices (Int, Int)
                 | NoDeduce
                 | Completed Sudoku
                 deriving (Show, Eq)

type SudokuProcessor = ExceptT SudokuError (State SudokuState)

availableNum :: Index Sudoku -> SudokuProcessor [Int8]
availableNum p = do
    s <- get
    if board s ! p == 0
    then let candidates = sudokuSetToList $ s `query` p
          in case candidates of
            [] -> throwError (NoChoices p)
            _  -> return candidates
    else return []

checkCompleted :: SudokuProcessor ()
checkCompleted = do
    completed <- gets (U.notElem 0 . unSudoku . board)
    when completed $ do
        s <- gets board
        throwError $ Completed s

deduce :: SudokuProcessor Sudoku
deduce = do
    checkCompleted
    candidates <- traverse availableNum sudokuIndices
    let singular = V.findIndices onlyOneChoice candidates
    if V.null singular
    then throwError NoDeduce
    else do
        let updates :: V.Vector SudokuChoice
            updates = V.zip (V.backpermute sudokuIndices singular)
                        (head <$> V.backpermute candidates singular)
        maybe
            (do
                modify' (`updateChoices` V.toList updates)
                gets board
            )
            (throwError . NoChoices)
            (isConsistent updates)
  where
    onlyOneChoice [_] = True
    onlyOneChoice _  = False
    -- isConsistent v returns Nothing if the update list is consistent,
    -- and Just a if it is not, where a is the address of an inconsistency.
    -- This is necessary because we do the deduction in effectively in parallel
    -- and we make guesses instead of pure deduction.
    -- The deductions are made in parallel in the sense that the update list
    -- is generated before the state is updated.
    -- If the board is correct but partially fill, then the checks always pass.
    -- However, if we have guessed incorrectly then the checks would be necessary
    -- to prevent incorrect deductions.
    isConsistent :: V.Vector SudokuChoice -> Maybe (Index Sudoku)
    isConsistent v
      | V.length v == 1 = Nothing
      | otherwise       = isConsistentRow <|> isConsistentCol <|> isConsistentBox
      where
        -- V.findIndices p u returns the indices of elements satisfying
        -- the predicate in *ascending order*.
        -- Hence, v is already sorted by row then column and
        -- there is no need to preprocess it unlike in the
        -- column and box cases.
        isConsistentRow = foldr go Nothing $ V.zip v (V.tail v)
          where
            go ((ax@(rx, _), dx), ((ry, _), dy)) acc
              | rx == ry && dx == dy = Just ax
              | otherwise = acc
        isConsistentCol = foldr go Nothing
                        $ (\ u -> V.zip u (V.tail u))
                        $ V.modify V.IS.sort v
          where
            go ((ax@(_, cx), dx), ((_, cy), dy)) acc
              | cx == cy && dx == dy = Just ax
              | otherwise = acc
        isConsistentBox = foldr go Nothing
                        $ (\ u -> V.zip u (V.tail u))
                        $ V.modify V.IS.sort
                        $ V.map (mapFst toBox) v
          where
            toBox (r, c) = (3*m + n, 3*s + t)
              where
                (m, s) = r `quotRem` 3
                (n, t) = c `quotRem` 3
            go ((ax@(bx, _), dx), ((by, _), dy)) acc
              | bx == by && dx == dy = Just ax
              | otherwise = acc


deduceAll :: SudokuProcessor ()
deduceAll = deduce >> deduceAll

guess :: SudokuChoice -> SudokuProcessor ()
guess p = modify' (`updateChoices` [p])

fill :: SudokuProcessor ()
fill = do
    deduceAll `catchError` continueOnNoDeduce
    save <- get
    sudokuPage <- V.zip sudokuIndices <$> traverse availableNum sudokuIndices
    let candidates = V.filter ((/= 0) . fst) $ toFst (length . snd) <$> sudokuPage
        (a, cs) = snd $ findGoodCell candidates
    -- We are abusing error catching here.
    -- We try each choice in turn.
    -- If a choice results in a completed puzzle, use the semantics of ExceptT
    -- to exit early.
    -- Otherwise, try the next choice.
    -- If all choices fail, throw an NoChoices to pass it up the stack.
    forM_ cs (\ c -> do
        let revertOnFail e = case e of
                Completed s -> throwError $ Completed s
                _           -> put save
        (guess (a, c) >> fill) `catchError` revertOnFail
             )
    throwError $ NoChoices a
  where
    continueOnNoDeduce e = case e of
        NoDeduce -> return ()
        _        -> throwError e
    -- A good cell is a cell that has a minimum amount of choices.
    -- After deduceAll, all cells must at least have two choices.
    -- If we find a cell with exactly two choices, we can exit early.
    findGoodCell xs = foldr1 go xs
      where
        go x@(lx, _) acc@(ly, _)
          | lx == 2   = x
          | lx < ly   = x
          | otherwise = acc


groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = as : groupsOf n bs
    where
      (as, bs) = splitAt n xs

extractor :: Either SudokuError () -> Int
extractor (Left (Completed s)) = toInt $ U.slice 0 3 $ unSudoku s
  where
    toInt :: U.Vector Int8 -> Int
    toInt = U.foldl' (\ acc d -> acc*10 + fromIntegral d) 0
extractor _ = errorWithoutStackTrace "extractor: failed to extract"



main :: IO ()
main = do
    file <- readFile "p096_sudoku.txt"
    let sudokuList = map ( sudokuState
                         . sudoku
                         . map (fromIntegral . digitToInt)
                         . concat
                         . tail
                         )
                     .  groupsOf 10 . lines $ file
    print $ sum $ map (extractor . evalState (runExceptT fill)) sudokuList


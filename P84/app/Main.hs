module Main where


import Data.IntMap qualified as IM

import Data.List (sortOn)
import Data.Ord

import System.Random

import Data.Foldable (foldl')
import Control.Monad
import Control.Monad.State

import Distributions qualified as D

communityChest :: D.DistInt
-- There are exactly 40 squares on a Monopoly board.
-- We can number those squares from 0 to 39.
-- Therefore, to avoid any confusion with the number 40,
-- we can use 41 as the "do not move" option.
communityChest =
    D.simpleDist
        ( IM.fromAscList [ (0, 1)    -- Advance to GO
                         , (10, 1)   -- Go to Jail
                         , (41, 14)  -- Stay on current square
                         ]
        )

chance :: D.DistInt
chance =
    D.simpleDist
        ( IM.fromAscList [ (0, 1)   -- Advance to GO
                         , (5, 1)   -- Go to R1
                         , (10, 1)  -- Go to Jail
                         , (11, 1)  -- Go to C1
                         , (24, 1)  -- Go to E3
                         , (38, 1)  -- Go to H2
                         , (41, 6)  -- Stay on the current square
                         , (42, 2)  -- Go to next R
                         , (43, 1)  -- Go to next U
                         , (44, 1)  -- Go back 3 squares
                         ]
        )

nextR :: Square -> Square
-- Given the current square, returns the next Railway square.
nextR sq
  | sq <= 5   = 5
  | sq <= 15  = 15
  | sq <= 25  = 25
  | sq <= 35  = 35
  | otherwise = 5   -- Otherwise we have passed R4 and the next R is on square 5.

nextU :: Square -> Square
-- Given the current square, returns the next Railway square.
nextU sq
  | sq <= 12  = 12
  | sq <= 28  = 28
  | otherwise = 12   -- Otherwise we have passed U2 and the next U is on square 12.


data MonopolyDiceState =
    MonopolyDiceState { doublesCount  :: Int
                      , currentStdGen :: StdGen
                      } deriving (Show, Eq)

twodN :: Int -> State StdGen (Int, Int)
twodN n = do
    let dN = D.die n
    a <- D.sample dN
    b <- D.sample dN
    return (a, b)

type MonopolyDice = State MonopolyDiceState Int

-- We note that the "promoteToX" functions can technically be subsumed
-- by a single promote function.
--
-- INVALID HASKELL CODE:
-- promote :: (t -> s) -> State s a -> State t a
-- promote p m = do
--     s <- gets p
--     let (a, ns) = runState m s
--     modify (putWith p ns)
--     return a
--   where
--     putWith p ns = s { p = ns }
--
-- However, s { f = ns } may or may not be valid Haskell.
-- E.g. t :: Integer, s :: Int.
--
-- We can solve this by augmenting the projection p with an embedding.
-- promote :: (t -> s) -> (s -> t -> t) -> State s a -> State t a
-- promote p e m = do
--     s <- gets p
--     let (a, ns) = runState m s
--     modify (e ns)
--     return a
--
--  Turns out this is what exactly lens are.
--  data Lens a b = Lens (a -> b) (b -> a -> a)
--  get (Lens g _) = g
--  put (Lens _ s) = s
--
--  However, since there are exactly two promoteToX functions I won't bother
--  with lens.

promoteToDS :: State StdGen a -> State MonopolyDiceState a
-- promoteToDS promotes a state processor acting on StdGen
-- to a state processor acting on the currentStdGen section of
-- MonopolyDiceState while keeping doublesCount unchanged.
promoteToDS d = do
    g <- gets currentStdGen
    let (r, ng) = runState d g
    modify (updateStdGen ng)
    return r
  where
    updateStdGen ng ds = ds { currentStdGen = ng }

monopoly2dN :: Int -> MonopolyDice
monopoly2dN n = do
    c <- gets doublesCount
    (a, b) <- promoteToDS (twodN n)

    let newCount = if a /= b then 0 else c + 1
    if newCount < 3
    then do
        modify (updateCount newCount)
        return (a + b)
    else do
        modify (updateCount 0)
        return 0  -- 0 is the value for jail.
  where
    updateCount nc ds = ds { doublesCount = nc }


type Square = Int

data MonopolyGameState =
    MonopolyGameState { currentSquare    :: Square
                      , currentDiceState :: MonopolyDiceState
                      } deriving (Show, Eq)

promoteToGS :: State MonopolyDiceState a -> State MonopolyGameState a
-- promoteToGS promotes a state processor acting on MonopolyDiceState
-- to a state processor acting on the currentDiceState section of
-- MonopolyGameState while keeping currentSquare unchanged.
promoteToGS d = do
    ds <- gets currentDiceState
    let (r, nds) = runState d ds
    modify (updateDiceState nds)
    return r
  where
    updateDiceState nds gs = gs { currentDiceState = nds }

initialGameState :: Int -> MonopolyGameState
initialGameState n =
    MonopolyGameState { currentSquare = 0
                      , currentDiceState = initialDiceState
                      }
  where
    initialDiceState =
        MonopolyDiceState { doublesCount = 0
                          , currentStdGen = mkStdGen n
                          }


nextTurn :: MonopolyDice -> State MonopolyGameState Square
nextTurn d = do

    r <- promoteToGS d
    sq <- gets currentSquare
    let nsq = (sq + r) `rem` 40

    let runNormal :: State MonopolyGameState Square
        runNormal = moveToSquare nsq

    let runCommunityChest :: State MonopolyGameState Square
        runCommunityChest = do
            cc <- (promoteToGS . promoteToDS . D.sample) communityChest
            case cc of
              41 -> moveToSquare nsq  -- Stay on current square
              10 -> moveToJail
              _  -> moveToSquare cc   -- Move to GO effectively

    let runChance :: State MonopolyGameState Square
        runChance = do
            ch <- (promoteToGS . promoteToDS . D.sample) chance
            case ch of
              41 -> moveToSquare nsq  -- Stay on current square
              42 -> moveToSquare (nextR nsq)
              43 -> moveToSquare (nextU nsq)
              44 -> moveToSquare (nsq - 3)
              10 -> moveToJail
              _  -> moveToSquare ch   -- Otherwise move to the square on Chance card

    if r /= 0
    then do
        case nsq of
          2  -> runCommunityChest
          17 -> runCommunityChest
          33 -> runCommunityChest
          7  -> runChance
          22 -> runChance
          36 -> runChance
          30 -> moveToJail
          _  -> runNormal
    else moveToJail

  where
    moveToJail :: State MonopolyGameState Square
    moveToJail = do
        modify updateStateJail
        moveToSquare 10  -- 10 is the Jail square
      where
        updateStateJail s =
            s { currentDiceState = (currentDiceState s) { doublesCount = 0 } }

    moveToSquare :: Square -> State MonopolyGameState Square
    moveToSquare sq = do
        modify updateSquare
        return sq
      where
        updateSquare gs = gs { currentSquare = sq }


buildResult :: [Int] -> IM.IntMap Integer
buildResult = foldl' (\ m k -> IM.insertWith (+) k 1 m) IM.empty

runSimulation :: Int -> Int -> Int -> [(Int, Integer)]
-- Run simulation using two n-sided dice, an initial state of s,
-- and for number of turns t.
runSimulation n s t =
    sortOn (Down . snd) $
    IM.toList $
    buildResult $
    evalState
        (replicateM t (nextTurn (monopoly2dN n)))
        (initialGameState s)


main :: IO ()
main = do
    putStr "D6: "
    print $ runSimulation 6 0 (10^6)
    putStr "D4: "
    print $ runSimulation 4 0 (10^6)


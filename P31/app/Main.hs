module Main where

import Control.Monad (guard)

coins :: [Int]
coins = [1,2,5,10,20,50,100,200]

splitter :: Int -> Int -> [[Int]]
splitter n cur = do
    coin <- coins
    guard $ (coin <= n) && (coin >= cur)

    if coin == n
    then return [coin]
    else do
        next <- splitter (n-coin) coin
        return $ coin : next



main :: IO ()
main = print $ length (splitter 200 0)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Mod.Word
import GHC.TypeLits (type (^))

answer :: Mod (10^10)
answer = 28433 * 2^%(7830457 :: Int) + 1


main :: IO ()
main = print answer

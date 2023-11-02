module Main where
import Data.Char (digitToInt)

isFifth :: Int -> Bool
isFifth n = sum (map ((^5).digitToInt) (show n)) == n

main :: IO ()
main = print $ sum (filter isFifth [2..1000000])

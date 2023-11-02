{-# LANGUAGE BangPatterns #-}


module Main where


romanCharToInt :: Char -> Int
romanCharToInt c =
    case c of
      'I' -> 1
      'V' -> 5
      'X' -> 10
      'L' -> 50
      'C' -> 100
      'D' -> 500
      'M' -> 1000
      _   -> errorWithoutStackTrace "romanToInt: invalid Roman numeral"

zipAhead :: [a] -> [(a, a)]
zipAhead xs = zip xs (tail xs)

romanToInt :: String -> Int
romanToInt = go 0 . map romanCharToInt
  where
    go :: Int -> [Int] -> Int
    go acc [] = acc
    go acc [a] = acc + a
    go acc (x:xs@(y:xss)) =
        if x >= y
        then go (acc+x) xs
        else go (acc+y-x) xss

intToRomanList :: [(Int, String)]
intToRomanList =
    [ (1000, "M" )
    , ( 900, "CM")
    , ( 500, "D" )
    , ( 400, "CD")
    , ( 100, "C" )
    , (  90, "XC")
    , (  50, "L" )
    , (  40, "XL")
    , (  10, "X" )
    , (   9, "IX")
    , (   5, "V" )
    , (   4, "IV")
    , (   1, "I" )
    ]

intToRoman :: Int -> String
intToRoman = go intToRomanList
  where
    go _  0 = []
    go [] _ = errorWithoutStackTrace "intToRoman: wtf"
    go ((x, str):xs) n = concat (replicate q str) ++ go xs (n - q*x)
      where
        q = n `quot` x

minimumRoman :: String -> String
minimumRoman = intToRoman . romanToInt

main :: IO ()
main = do
    file <- readFile "p089_roman.txt"
    let lined = lines file
        minimumLength = sum $ map (length . minimumRoman) lined
        originalLength = sum $ map length lined
    print (originalLength - minimumLength)

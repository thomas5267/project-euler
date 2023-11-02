import Data.Char

digitSum :: Integer -> Integer

digitSum n = sum $ map (toInteger.digitToInt) $ show n

result = digitSum (2^1000)
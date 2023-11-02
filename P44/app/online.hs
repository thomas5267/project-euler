import qualified Data.Set as Set

main = print (take 1 solutions)
 where solutions = [a-b | a <- penta, b <- takeWhile (<a) penta,
                          isPenta (a-b), isPenta (b+a)]
       isPenta = (`Set.member` Set.fromList  penta)
       penta = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]

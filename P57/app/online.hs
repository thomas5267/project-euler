import Data.Ratio

expansions x = reverse . map ((1%1) `fn`) . scanr1 fn $ 
               (replicate x (2%1))

fn :: Rational -> Rational -> Rational
fn a b = a + 1/b

digits :: Integer -> Int
digits = length . show

test x = digits (numerator x) > digits (denominator x)

main = print (length . filter test $ expansions 1000)


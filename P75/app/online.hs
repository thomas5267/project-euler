
import qualified Data.Map as M

primitiveTriples :: [(Int, Int)]
primitiveTriples = [(m*m - n*n, 2*m*n) |
    m <- [1..], n <- [1..m], mod n 2 /= mod m 2, gcd m n == 1]

lim :: Int
lim = 15*10^5

sol :: Int
sol = length $ filter ((==1) . snd) $ M.toList $ foldr (uncurry $ M.insertWith (+)) M.empty [
    (mp, 1) |
    (a, b) <- takeWhile ((<=lim) . uncurry (+)) primitiveTriples,
    let c = floor (sqrt $ fromIntegral (a^2 + b^2)),
    let p = a + b + c,
    mp <- takeWhile (<=lim) $ iterate (p+) p]

main = print sol

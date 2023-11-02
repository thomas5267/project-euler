coins = [1,2,5,10,20,50,100,200]

combinations = foldl (\without p ->
                          let (poor,rich) = splitAt p without
                              with = poor ++ zipWith (++) (map (map (p:)) with)
                                                          rich
                          in with
                     ) ([[]] : repeat [])

-- [[]] is a zero-based index offset

helper without p = let (poor,rich) = splitAt p without
                       with = poor ++ zipWith (++) (map (map (p:)) with)
                                                   rich
                    in with
-- Use splitAt to determine where to modify

problem_31 = length $ combinations coins !! 200

-- withcoins 1 x = [[x]]
-- withcoins n x = concatMap addCoin [0 .. x `div` coins!!(n-1)]
--   where addCoin k = map (++[k]) (withcoins (n-1) (x - k*coins!!(n-1)) )
-- 
-- problem_31 = length $ withcoins (length coins) 200

main = print problem_31

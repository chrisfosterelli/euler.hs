main = print . sum $ [ a | a <- [1..999], 0 `elem` fmap (mod a) [3, 5] ]

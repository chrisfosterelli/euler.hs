choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = n * choose (n - 1) (k - 1) `div` k

maze :: Int -> Int
maze n = choose (n*2) n

main = print . maze $ 20

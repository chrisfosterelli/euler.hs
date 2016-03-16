collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n    = 1 + collatz (n `div` 2)
  | otherwise = 1 + collatz (n * 3 + 1)

collatzMax :: Int -> Int -> Int
collatzMax a n = if collatz a > collatz n 
                 then a 
                 else n

main = print . foldl1 collatzMax $ [1..1000000]

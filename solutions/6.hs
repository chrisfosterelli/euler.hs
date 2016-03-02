difference :: Integer -> Integer
difference n = let squareOfSum = sum [1..n] ^ 2
                   sumOfSquares = sum . map (^2) $ [1..n]
               in squareOfSum - sumOfSquares
main = print . difference $ 100

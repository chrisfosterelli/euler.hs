import Data.Char

sumDigits :: Integer -> Int
sumDigits x = sum . map digitToInt . show $ x

main = print . sumDigits $ (2^1000)

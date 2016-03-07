findA :: (Fractional a) => a -> a
findA b = (500000 - 1000 * b) / (1000 - b)

isNatural :: (RealFrac a, Eq a) => a -> Bool
isNatural x = x == fromInteger (round x)

solutions = [ b | b <- [1..], isNatural . findA $ b ]

main = print . truncate $ a * b * c
  where a = findA b
        b = head solutions
        c = sqrt (a^2 + b^2)

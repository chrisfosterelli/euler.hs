evenlyDivisibleBy :: [Integer] -> [Integer]
evenlyDivisibleBy a = [ n | n <- [1..], all (== 0) . map (mod n) $ a ]
main = print . head . evenlyDivisibleBy $ [1..20]

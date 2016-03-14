import Math.NumberTheory.Primes.Factorisation
triangulars = scanl (+) 1 [2..]
main = print . head . filter ((>500) . length . divisors) $ triangulars

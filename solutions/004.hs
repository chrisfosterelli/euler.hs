palidrome :: String -> Bool
palidrome s = take l s == reverse(drop l s)
  where l = (length s `quot` 2)
products = [ n * m | n <- [100..999], m <- [100..999] ]
main = print . maximum . filter palidrome . map show $ products

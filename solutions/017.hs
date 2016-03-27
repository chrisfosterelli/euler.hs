toWords :: Int -> String
toWords n = thousands ++ hundreds ++ and ++ end
  where thousands = thousandsMap . quot n $ 1000
        hundreds  = hundredsMap . quot (rem n 1000) $ 100
        tens      = tensMap . quot (rem n 100) $ 10
        teens     = teensMap . rem n $ 100
        ones      = onesMap . rem n $ 10
        end       = if rem n 100 <= 10 || rem n 100 >= 20
                    then tens ++ ones
                    else teens 
        and       = if rem n 100 /= 0 && n > 99
                    then "and"
                    else ""

hundredsMap :: Int -> String
hundredsMap n 
  | n > 0     = onesMap n ++ "hundred"
  | otherwise = ""

thousandsMap :: Int -> String
thousandsMap n
  | n > 0     = onesMap n ++ "thousand"
  | otherwise = ""

tensMap :: Int -> String
tensMap n
  | n == 1    = "ten"
  | n == 2    = "twenty"
  | n == 3    = "thirty"
  | n == 4    = "forty"
  | n == 5    = "fifty"
  | n == 6    = "sixty"
  | n == 7    = "seventy"
  | n == 8    = "eighty"
  | n == 9    = "ninety"
  | otherwise = "" 

teensMap :: Int -> String
teensMap n
  | n == 11   = "eleven"
  | n == 12   = "twelve"
  | n == 13   = "thirteen"
  | n == 14   = "fourteen"
  | n == 15   = "fifteen"
  | n == 16   = "sixteen"
  | n == 17   = "seventeen"
  | n == 18   = "eighteen"
  | n == 19   = "nineteen"
  | otherwise = ""

onesMap :: Int -> String
onesMap n
  | n == 1    = "one"
  | n == 2    = "two"
  | n == 3    = "three"
  | n == 4    = "four"
  | n == 5    = "five"
  | n == 6    = "six"
  | n == 7    = "seven"
  | n == 8    = "eight"
  | n == 9    = "nine"
  | otherwise = ""

main = print . sum . map (length . toWords) $ [1..1000]

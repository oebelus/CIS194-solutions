-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

revDigits :: [Integer] -> [Integer]
revDigits = foldl (flip (:)) []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (revDigits . toDigits) x

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = revDigits . process . revDigits
  where
    process [] = []
    process [x] = [x]
    process (x : y : tail) = x : (y * 2) : process tail

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = foldl (\acc x -> sum (toDigits x) + acc) 0

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

main :: IO ()
main = print (validate 4012888888881882)

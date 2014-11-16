{-
Name: Viet Nguyen 
Notes:  
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.
-- Credite card validate 
lastDigit :: Integer -> Integer
lastDigit a = a `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit a = a `div` 10 

toDigits :: Integer -> [Integer]
toDigits a
    | a <= 0 = []
    | otherwise = (toDigits $ dropLastDigit a) ++ [(lastDigit a)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a = 
    let b = reverseList a
    in reverseList (doubleEveryOther' b)

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' (x1:x2:xs) = x1 : (x2 * 2) : doubleEveryOther' xs
doubleEveryOther' (x:[]) = x : []

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (foldl (+) 0 $ toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate a = 
    let sum = sumDigits . doubleEveryOther $ toDigits a
    in sum `mod` 10 == 0

-- Exercise 6: The Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a) 

-- Exercise 7: The Tower of Hanoi, with four pegs 
hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]

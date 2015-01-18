module HW07 where 

-- Exercise 1 
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 
fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)

-- take 3 (fibs2) 
-- = take 3 ([0,1] ++ zipWith (+) fibs2 (tail fibs2))
-- = 0 : 1 : take 1 (zipWith (+) fibs2 (tail fibs2))
-- = 0 : 1 : take 1 (zipWith (+) ([0,1]..) ([1]...))
-- = 0 : 1 : 1 : []

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

-- Exercise 4 
instance Show a => Show (Stream a) where 
    show s = take 20 (streamToList s)
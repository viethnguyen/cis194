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
    show s = show $ take 20 (streamToList s)

-- Exercise 5 
streamRepeat :: a -> Stream a 
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a->a) -> a -> Stream a 
streamFromSeed f x = Cons x (streamMap f (streamFromSeed f x))

-- to test, try this: 
-- show (streamFromSeed ('x':) "o")
-- show (streamRepeat "o")
-- show (streamMap (+ 1) (streamRepeat 0))




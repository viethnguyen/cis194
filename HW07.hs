{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module HW07 where 

import System.Random 

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

-- Exercise 6 
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0 

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 s1) ~(Cons x2 s2) = Cons x1 (Cons x2 (interleaveStreams s1 s2))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

-- Exercise 7
randomList :: (Random a, RandomGen g) => g -> [a]
randomList g = (\(x,g') -> x : randomList g') (random g)

-- Exercise 8 
randomInts :: Int -> [Int]
randomInts n = take n $ randomList (mkStdGen 2)

-- Exercise 9
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing -- no min or max if there is no elements
minMax xs = Just (minimum xs , maximum xs)

-- Result of minMax (strict) version
-- Total memory usage: 457 MB 

-- Exercise 10 
minMaxLazy :: [Int] -> Maybe (Int, Int)
minMaxLazy [] = Nothing 
minMaxLazy (x:xs) = case minMaxLazy xs of 
    Nothing -> Just (x,x)
    Just (a,b) -> Just (min x a, max x a)

-- Result of minMax (lazy) version
-- Total memory usage: 286 MB 

main :: IO ()
main = print (minMaxLazy (randomInts 1000000))

-- Exercise 11 
data Matrix = Elem Integer Integer Integer Integer
    deriving Show

instance Num Matrix where 
    (*) (Elem x1 x2 x3 x4) (Elem y1 y2 y3 y4) = 
        (Elem (x1*y1 + x2*y3) (x1*y2 + x2*y4) (x3*y1 + x4*y3) (x3*y2 + x4*y4))

fib4 :: Integer -> Integer 
fib4 0 = 0 
fib4 n = getFn ((Elem 1 1 1 0) ^ n)
    where getFn (Elem x1 x2 x3 x4) = x2 
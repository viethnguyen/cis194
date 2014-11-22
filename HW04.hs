module HW04 where

import BST
import Data.Maybe
import Data.Char

ex1 :: a -> b -> b
ex1 x y = y

ex2 :: a -> a -> a
ex2 x y = x

ex3 :: Int -> a -> a
ex3 x y = y

ex4 :: Bool -> a -> a -> a
ex4 b x y = x

ex5 :: Bool -> Bool
ex5 True = False
ex5 _ = True

ex6 :: (a->a) -> a
ex6 = error "Impossible"

ex7 :: (a->a) -> a -> a 
ex7 f x  = f x 

ex8 :: [a] -> [a]
ex8 [] = []
ex8 (x: xs) = x : ex8 xs

ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
ex9 f (x:xs) = f x : (ex9 f xs)

-- We cannot get data of type a when the input is Nothing, so ex10 is impossible
ex10 :: Maybe a -> a 
ex10 = error "Impossible"

ex11 :: a -> Maybe a 
ex11 x = Just x 

ex12 :: Maybe a -> Maybe a
ex12 Nothing = Nothing
ex12 (Just x) = Just x 

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a 
insertBST order x Leaf = Node Leaf x Leaf 
insertBST order x (Node left y right) = case (order x y) of 
                                            LT -> Node (insertBST order x left) y right
                                            GT -> Node left y (insertBST order x right)
                                            EQ -> Node left y right

safeHead :: [a] -> Maybe a 
safeHead [] = Nothing
safeHead (x:_) = Just x 

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- This function chains several built-in functions together to form a one-line solution
allCaps :: [String] -> Bool 
allCaps words = foldl (&&) True (map (\s -> isUpper(fromMaybe 'a' (safeHead s)) ) words)
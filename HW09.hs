{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import Test.HUnit
import Data.List
import Control.Monad 
import BST
import Ring
import Control.Monad.Random


-- !Assuming the input lists are sorted, combine the lists into a
-- sorted output 
merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys) 
    | x < y     = x : merge1 xs ys
    | otherwise = y : merge1 xs ys
merge1 _    _   = []

test1_merge1 :: Test 
test1_merge1 = "alternating numbers: [1,3,5] [2,4,6]" ~:
                merge1 [1,3,5][2,4,6] ~?= [1,2,3,4,5,6]

test2_merge1 :: Test 
test2_merge1 = TestList ["one element: [1] []" ~:
                         merge1 [1][] ~?= [1]
                        , test1_merge1 ]

type MergeFun = Ord a => [a] -> [a] -> [a]
test2 :: MergeFun -> Test 
test2 merge  = TestList ["one element [1] []" ~:
                         merge [1] [] ~?= [1]
                        , "alternating numbers: [1,3,5] [2,4,6]" ~:
                         merge [1,3,5][2,4,6] ~?= [1,2,3,4,5,6]
                        ]

merge2 :: MergeFun 
merge2 all_xs@(x:xs) all_ys@(y:ys) 
    | x < y     = x : merge2 xs all_ys
    | otherwise = y : merge2 all_xs ys
merge2 _    _   = []

merge3 :: MergeFun 
merge3 all_xs@(x:xs) all_ys@(y:ys)
    | x < y         =  x : merge3 all_ys xs
    | otherwise     =  y : merge3 all_xs ys 
merge3 xs   []      = xs
merge3 _    _       = []

test3 :: MergeFun -> Test 
test3 merge = "empty lists: [] []" ~:
                merge [] [] ~?= ([] :: [Integer])

prop_numElements_merge3 :: [Integer] -> [Integer] -> Bool 
prop_numElements_merge3 xs ys 
    = length xs + length ys == length (merge3 xs ys)

prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool 
prop_numElements merge xs ys 
    = length xs + length ys == length (merge xs ys) 

merge4 :: MergeFun
merge4 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge4 xs all_ys
  | otherwise = y : merge4 all_xs ys
merge4 xs            ys            = xs ++ ys

prop_sorted1 :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_sorted1 merge xs ys 
    = merge xs ys == sort (xs ++ ys)

prop_sorted2 :: MergeFun -> [Integer] -> [Integer] -> Property
prop_sorted2 merge xs ys 
    = isSorted xs && isSorted ys ==> merge xs ys == sort (xs ++ ys)

isSorted :: Ord a => [a] -> Bool
isSorted (a:b:rest)  =  a <= b && isSorted (b:rest)
isSorted _           = True 

prop_sorted3 :: MergeFun -> OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted3 merge (Ordered xs) (Ordered ys)
    = merge xs ys == sort (xs ++ ys)



-- Beginning homework 
-- Exercise 1 
instance Arbitrary Mod5 where 
  arbitrary = do 
    n <- choose (0, 4) :: Gen Integer
    return (MkMod n) 

instance Arbitrary Mat2x2 where 
  arbitrary = do
    x1 <- getRandom :: IO Integer 
    x2 <- getRandom :: IO Integer  
    x3 <- getRandom :: IO Integer
    x4 <- getRandom :: IO Integer 
    return (MkMat x1 x2 x3 x4)




module HW05 where

import Ring
import Parser

import Data.Maybe    ( listToMaybe )

data Mod5 = MkMod Integer
    deriving (Show, Eq)

readsMod5 :: ReadS Mod5
readsMod5 ('0':s) = [(MkMod 0, s)]
readsMod5 ('1':s) = [(MkMod 1, s)]
readsMod5 ('2':s) = [(MkMod 2, s)]
readsMod5 ('3':s) = [(MkMod 3, s)]
readsMod5 ('4':s) = [(MkMod 4, s)]
readsMod5 _ = [] 

instance Parsable Mod5 where 
    parse = listToMaybe . readsMod5

instance Ring Mod5 where
    addId = MkMod 0
    addInv (MkMod x) = MkMod (5 - x)
    mulId = MkMod 1

    add (MkMod m) (MkMod n) = MkMod ((m + n) `mod` 5)
    mul (MkMod m) (MkMod n) = MkMod ((m * n) `mod` 5)

-- Test function for Mod5 data
mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "3" == Just (MkMod 3, "")) &&
                   (parse "345" == Just (MkMod 3, "45" )) &&
                   (parseRing " 3 + 4 " == Just(MkMod 2)) && 
                   (parseRing "2 + 3 * 4" == Just (MkMod 4)) && 
                   (addId == MkMod 0)

-- Exercise 3
data Mat2x2 = Elem Integer Integer Integer Integer
    deriving (Show, Eq)

getMatElem :: Mat2x2 -> Integer -> Integer
getMatElem (Elem x1 x2 x3 x4) pos 
    | pos == 1      = x1
    | pos == 2      = x2
    | pos == 3      = x3
    | pos == 4      = x4
    | otherwise     = 0


instance Ring Mat2x2 where 
    addId = Elem 0 0 0 0
    addInv (Elem x y z w) = Elem (-x) (-y) (-z) (-w)
    mulId = Elem 1 1 1 1

    add mat1 mat2 = Elem (getMatElem mat1 1 + getMatElem mat2 1)
                         (getMatElem mat1 2 + getMatElem mat2 2)
                         (getMatElem mat1 3 + getMatElem mat2 3)
                         (getMatElem mat1 4 + getMatElem mat2 4)
    mul mat1 mat2 =  Elem (getMatElem mat1 1 * getMatElem mat2 1)
                         (getMatElem mat1 2 * getMatElem mat2 2)
                         (getMatElem mat1 3 * getMatElem mat2 3)
                         (getMatElem mat1 4 * getMatElem mat2 4)

-- Exercise 4 
instance Parsable Bool where 
    parse = listToMaybe . reads

instance Ring Bool where 
    addId = False
    addInv = not
    mulId = True

    add = xor
    mul = (&&) 

xor :: Bool -> Bool -> Bool 
xor True False = True 
xor False True = True 
xor _ _ = False 

-- Test function for Bool data
boolParsingWorks :: Bool
boolParsingWorks = parse "True" == Just (True, "") &&
                   parse "False" == Just (False, "") &&
                   parseRing "True + False" == Just (True) && 
                   parseRing "True * False" == Just (False) && 
                   parseRing "False * False" == Just(False)

-- Exercise 5
distribute :: Ring a => RingExpr a -> a 
distribute Mul x (Add y z) = Add (Mul x y) (Mul x z)
distribute Mul (Add x y) z = Add (Mul x z) (Mul y z)
distribute m  = eval m   
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
                   parseRing "True + False" == Just True && 
                   parseRing "True * False" == Just False && 
                   parseRing "False * False" == Just False

-- Exercise 5
swapIdentities :: RingExpr a -> RingExpr a 
swapIdentities AddId = MulId 
swapIdentities MulId = AddId 
-- need other cases to do this *everywhere* in the expression: 
swapIdentities (Lit a) = Lit a 
swapIdentities (AddInv x) = AddInv (swapIdentities x)
swapIdentities (Add x y) = Add (swapIdentities x) (swapIdentities y)
swapIdentities (Mul x y) = Mul (swapIdentities x) (swapIdentities y)

distribute :: Ring a => RingExpr a -> RingExpr a
distribute (Mul x (Add y z)) = Add (Mul x y) (Mul x z) 
distribute (Mul (Add x y) z) = Add (Mul x z) (Mul y z)
distribute AddId = AddId 
distribute MulId = MulId 
distribute (Lit a) = Lit a 
distribute (AddInv x) = AddInv (distribute x) 
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (Mul x y) = Mul (distribute x) (distribute y)

--test 
distributeWorks :: Bool 
distributeWorks = fmap distribute (parseRing "2 * (1 +2)" :: Maybe (RingExpr Integer)) 
                    == fmap distribute (parseRing "2 * 1 + 2 *2" :: Maybe (RingExpr Integer)) && 
                  fmap distribute (parseRing "(3 + 4) * 5" :: Maybe (RingExpr Integer))
                    == fmap distribute (parseRing "3 * 5 + 4 * 5" :: Maybe (RingExpr Integer))

-- Exercise 6
squashMulId :: Ring a => RingExpr a -> RingExpr a
squashMulId (Mul MulId x) =   x 
squashMulId (Mul x MulId) = x
-- need other cases to do this *everywhere* in the expression
squashMulId (AddId) = AddId 
squashMulId (MulId) = MulId 
squashMulId (Lit a) = Lit a 
squashMulId (AddInv x) = AddInv (squashMulId x)
squashMulId (Add x y) = Add (squashMulId x) (squashMulId y)
squashMulId (Mul x y) = Mul (squashMulId x) (squashMulId y)

-- test - STILL NOT PASSED!
squashWorks :: Bool 
squashWorks = fmap squashMulId (parseRing "(3+4) * 1" :: Maybe (RingExpr Integer)) 
                == fmap squashMulId (parseRing "3 + 4" :: Maybe (RingExpr Integer))
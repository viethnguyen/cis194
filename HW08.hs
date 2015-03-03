module Main where 

import Text.Read
import Data.List 
import Data.Maybe 
import Control.Monad
import Control.Monad.Random
import Data.Monoid

-- Exercise 1 
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
    where 
        go :: String -> Maybe String
        go [] = Just [] 
        go s = case readMaybeInt [head(s)] of 
            Just n -> stripPrefix (replicate n 'a') (tail s) >>= go  
            Nothing -> Nothing
                -- go evaluates  to 'Just ""' on success, or Nothing otherwise 

readMaybeInt :: String -> Maybe Int 
readMaybeInt = readMaybe 

-- Exercise 2 
specialNumbers :: [Int]
specialNumbers = [n | n <- [1..100], ((mod n 5) == 0), ((mod n 7) /=0)]

-- Exercise 3 
type StdRand = Rand StdGen 
type Army = Int 
data ArmyCounts = ArmyCounts {attackers :: Army, defenders :: Army}
    deriving Show

type DieRoll = Int 

dieRoll :: StdRand DieRoll 
dieRoll = getRandomR (1, 6)

-- Exercise 4 
instance Monoid ArmyCounts where 
    mempty = ArmyCounts {attackers = 0, defenders = 0} 
    mappend (ArmyCounts {attackers = a1, defenders = d1})
        (ArmyCounts {attackers = a2, defenders = d2})
        = (ArmyCounts {attackers = a1 + a2, defenders = d1 + d2})


battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults [] _ = mempty
battleResults _ [] = mempty
battleResults x y = change <> (battleResults x_remainder y_remainder)
    where
        x1 = head(reverse $ sort x)
        y1 = head(reverse $ sort y)
        x_remainder = tail(reverse $ sort x)
        y_remainder = tail(reverse $ sort y) 
        change 
            | x1 > y1 = ArmyCounts {attackers = 0, defenders = -1} 
            | otherwise = ArmyCounts{attackers = -1, defenders = 0} 

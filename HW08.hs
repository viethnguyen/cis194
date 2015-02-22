module Main where 

import Text.Read
import Data.List 
import Data.Maybe 
import Control.Monad
import Control.Monad.Random

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
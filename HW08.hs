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

-- Exercise 5 
battle :: ArmyCounts -> StdRand ArmyCounts 
battle (ArmyCounts {attackers = a, defenders = d}) = do 
    nAttacks <- getRandomR (1, (min (a - 1) 3) )    -- random number of units for attackers
    nDefends <- getRandomR (1, (min (d - 1) 3) )    -- random number of units for defenders
    attackRolls <- dice nAttacks    -- get the list of random rolls
    defenseRolls <- dice nDefends   -- get the list of random rolls
    let result = battleResults attackRolls defenseRolls   -- calculate the outcome of this battle
    return ArmyCounts {attackers = a + (attackers result), defenders = d + (defenders result)}  -- caculate the new unit number for both.


-- The dice function uses replicate and sequence to simulate the roll of n dice 
-- We already have the dieRoll of the type `StdRand DieRoll`, so here we only needs to replicate them and sequence together
-- to get just one monadic value 
dice :: Int -> StdRand [DieRoll]
dice n = sequence (replicate n dieRoll) 

-- To extract a value from the Rand monad, we can use the `evalRandIO`
-- Example: evalRandIO (dice  4)

-- Exercise 6 
invade :: ArmyCounts -> StdRand ArmyCounts 
invade (ac@ArmyCounts {attackers = a, defenders = d}) 
    | d == 0 = return ac
    | a < 2 = return ac 
    | otherwise = do
        newac <- battle ac
        invade newac 

-- Exercise 7 
successProb :: ArmyCounts -> StdRand Double 
successProb ac = go ac 1000 0
    where go ac 0 y = return (y / 1000)
          go ac n y = do 
            finalac <- invade ac 
            if (defenders finalac == 0) then
                go ac (n-1) (y+1)
            else
                go ac (n-1) y 



(//) :: Int -> Int -> Double 
a // b = fromIntegral a / fromIntegral b
{-
Name: Viet Nguyen
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] hand = True
formableBy (x:xs) hand = (x `elem` hand) && (formableBy xs (delete x hand)) 

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate temp hand word = (matchTemplate temp word) && (formableBy word (temp ++ hand))

matchTemplate:: Template -> String -> Bool
matchTemplate [] [] = True
matchTemplate [] ws = False
matchTemplate ts [] = False
matchTemplate (x:xs) (y:ys) = (x == '?' || x == y) && (matchTemplate xs ys)

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate temp hand = filter (\word -> wordFitsTemplate temp hand word) allWords

scrabbleValueWord :: String -> Int 
scrabbleValueWord word = foldl (+) 0 (map scrabbleValue word)

bestWords :: [String] -> [String]
bestWords words = filter (\word -> scrabbleValueWord word == maxWords words) words

maxWords :: [String] -> Int
maxWords words = foldl (max) 0 (map scrabbleValueWord words)

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate t w = a * b 
    where a = multiplyWordBy t
          b = multiplyEachLetter t w

multiplyEachLetter :: STemplate -> String -> Int
multiplyEachLetter [] [] = 0
multiplyEachLetter (t:ts) (w:ws) 
    | t == 'D' = (2 * (scrabbleValue w)) + (multiplyEachLetter ts ws)
    | t == 'T' = (3 * (scrabbleValue w)) + (multiplyEachLetter ts ws)
    | otherwise = (scrabbleValue w) + (multiplyEachLetter ts ws)

multiplyWordBy :: STemplate -> Int
multiplyWordBy [] = 1
multiplyWordBy (t:ts) 
    | t == '2' = 2 * multiplyWordBy ts
    | t == '3' = 3 * multiplyWordBy ts
    | otherwise = multiplyWordBy ts

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log 
import Data.List (sortBy, isInfixOf)
import Data.Char (toUpper)

parseMessage :: String -> MaybeLogMessage
parseMessage s = case (head ws) of
                    "I" -> let timestamp = read (head(tail ws)) :: Int
                               message = unwords (drop 2 ws) 
                           in ValidLM (LogMessage Info timestamp message)
                    "W" -> let timestamp = read (head(tail ws)) :: Int
                               message = unwords (drop 2 ws)
                           in ValidLM (LogMessage Warning timestamp message)
                    "E" -> let errorLevel = read (head(tail ws)) :: Int
                               timestamp = read $ head $ tail $ tail ws :: Int 
                               message = unwords (drop 3 ws)
                           in ValidLM (LogMessage (Error errorLevel) timestamp message)
                    _ -> InvalidLM s
                 where ws = words s

validMessageOnly :: [MaybeLogMessage] -> [LogMessage]
validMessageOnly [] = []
validMessageOnly (x:xs) = case x of 
    InvalidLM _ -> validMessageOnly xs
    ValidLM lm -> lm : validMessageOnly xs

parse :: String -> [LogMessage]
parse s = validMessageOnly (map parseMessage $ lines s)

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs lm1 lm2 
    | time1 > time2 = GT 
    | time1 == time2 = EQ 
    | otherwise = LT 
    where time1 = getTimeStamp lm1
          time2 = getTimeStamp lm2

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ msgTime _) = msgTime

getMsgText :: LogMessage -> String
getMsgText (LogMessage _ _ msgText) = msgText

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages msgs = sortBy compareMsgs msgs 


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMsgText $ sortMessages (severeMsgsFilter msgs)
    where severeMsgsFilter ms = filter severeErrorMsgOnly ms
          severeErrorMsgOnly msg = case msg of
            LogMessage (Error severeLvl) _ _ -> if severeLvl >= 50 
                                                    then True
                                                    else False
            _ -> False  

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout text lms = filter mentionedOnly lms
    where mentionedOnly msg = (map toUpper text) `isInfixOf` (map toUpper (getMsgText msg))

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced text msgs = map getMsgText $ sortMessages (filter errorAndMentioned msgs)
    where errorAndMentioned = (|||) mentionedOnly severeErrorMsgOnly
          mentionedOnly msg = (map toUpper text) `isInfixOf` (map toUpper (getMsgText msg))  
          severeErrorMsgOnly msg = case msg of
            LogMessage (Error severeLvl) _ _ -> if severeLvl >= 50 
                                                    then True
                                                    else False
            _ -> False  

(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x
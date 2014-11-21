{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log 
import Data.List (sortBy)

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
    InvalidLM s -> validMessageOnly xs
    ValidLM lm -> lm : validMessageOnly xs

parse :: String -> [LogMessage]
parse s = validMessageOnly (map parseMessage $ lines s)

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs lm1 lm2 
    | time1 > time2 = GT 
    | time1 == time2 = EQ 
    | time1 < time2 = LT 
    where time1 = getTimeStamp lm1
          time2 = getTimeStamp lm2

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage msgType msgTime msgText) = msgTime



sortMessages :: [LogMessage] -> [LogMessage]
sortMessages msgs = sortBy compareMsgs msgs 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = 
    where severeMsgs = filter (\msg -> getTimeStamp msg > )
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log 

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
validMessageOnly = filter ()
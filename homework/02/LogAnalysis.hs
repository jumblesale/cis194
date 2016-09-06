{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char

import Log

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

parseMessage :: String -> LogMessage
parseMessage x
    | first == 'I' = parseInfo x
    | first == 'W' = parseWarning x
    | first == 'E' = parseError x
    | otherwise = Unknown x
    where first = toUpper . head $ x

parseInfo :: String -> LogMessage
parseInfo x = LogMessage Info (getIntAt x 1) (getRestOfMessage x 2)

parseWarning :: String -> LogMessage
parseWarning x = LogMessage Warning (getIntAt x 1) (getRestOfMessage x 2)

parseError :: String -> LogMessage
parseError x = LogMessage (Error (getIntAt x 1)) (getIntAt x 2) (getRestOfMessage x 3)

getIntAt :: String -> Int -> Int
getIntAt x i = read (words x !! i) ::Int

-- convert a log entry into just the message
-- "E 65 8 Bad pickle-flange interaction detected" -> "Bad pickle-flange interaction detected"
-- x: message
-- i: last index of whitespace
getRestOfMessage :: String -> Int -> String
-- getRestOfMessage x i = unwords (snd (splitAt i (words x)))
-- dot notation version:
getRestOfMessage x i = unwords . snd . splitAt i $ words x

-- insert a logmessage into a binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage1@(LogMessage _ t1 _) (Node left logMessage2@(LogMessage _ t2 _) right)
    | t1 > t2 = Node left logMessage2 (insert logMessage1 right)
    | otherwise = Node (insert logMessage1 left) logMessage2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [String]
inOrder Leaf = []
inOrder (Node left logMessage right) = (inOrder left) ++ [extractMessage logMessage] ++ (inOrder right)

severe :: LogMessage -> Bool
severe (LogMessage (Error e) _ _)
    | e > 50 = True
    | otherwise = False
severe _ = False

-- whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = inOrder . build . (filter severe)

-- get all the message from a node in a MessageTree
extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ message) = message
extractMessage (Unknown _) = ""

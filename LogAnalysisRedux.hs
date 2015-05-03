{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Now with less recursion!
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = ((map getMessage) . (filter isSevere) . inOrder . build) xs

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error level) _ _)
  | level >= 50 = True
  | otherwise   = False
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage _ = []

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ message : inOrder right

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert m1@(LogMessage _ t1 _) (Node left m2@(LogMessage _ t2 _) right)
  | t1 <= t2 = (Node (insert m1 left) m2 right)
  | t1 > t2  = (Node left m2 (insert m1 right))
insert _ _ = Leaf -- Bad tree?

parse :: String -> [LogMessage]
parse xs = map (parseMessage) (lines xs)

parseMessage :: String -> LogMessage
parseMessage xs = parseMessage' (words xs)

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":t:xs)   = LogMessage Info (read t) (unwords xs)
parseMessage' ("W":t:xs)   = LogMessage Warning (read t) (unwords xs)
parseMessage' ("E":c:t:xs) = LogMessage (Error (read c)) (read t) (unwords xs)
parseMessage' xs           = Unknown (unwords xs)

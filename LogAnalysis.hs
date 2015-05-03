{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = (whatWentWrong' . inOrder . build) xs

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage (Error level) _ message):xs)
  | level >= 50 = message : whatWentWrong' xs
  | otherwise   = whatWentWrong' xs
whatWentWrong' ((LogMessage _ _ _):xs) = whatWentWrong' xs
whatWentWrong' ((Unknown _):xs) = whatWentWrong' xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ message : inOrder right

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree        = tree
insert message Leaf = Node Leaf message Leaf
insert m1@(LogMessage _ t1 _) (Node left m2@(LogMessage _ t2 _) right)
  | t1 <= t2 = (Node (insert m1 left) m2 right)
  | t1 > t2  = (Node left m2 (insert m1 right))
insert (LogMessage _ _ _) (Node _ (LogMessage _ _ _) _) = Leaf
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = Leaf

parse :: String -> [LogMessage]
parse xs = parse' (lines xs)

parse' :: [String] -> [LogMessage]
parse' [] = []
parse' (x:[]) = parseMessage x : []
parse' (x:xs) = parseMessage x : parse' xs

parseMessage :: String -> LogMessage
parseMessage xs = parseMessage' (words xs)

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":t:xs)   = LogMessage Info (read t) (unwords xs)
parseMessage' ("W":t:xs)   = LogMessage Warning (read t) (unwords xs)
parseMessage' ("E":c:t:xs) = LogMessage (Error (read c)) (read t) (unwords xs)
parseMessage' xs           = Unknown (unwords xs)

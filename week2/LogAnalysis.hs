{-# OPTIONS_GHC -Wall #-}

-- Managing logs
module LogAnalysis where

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parser n file = take n . parser <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parser whatWentWronger file
  = whatWentWronger . parser <$> readFile file

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("E":n:t:msg) = LogMessage (Error (read n :: Int)) (read t :: Int) (unwords msg)
parseMessageWords ("I":t:msg) = LogMessage Info (read t :: Int) (unwords msg)
parseMessageWords ("W":t:msg) = LogMessage Warning (read t :: Int) (unwords msg)
parseMessageWords s = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not the right format" == Unknown "This is not the right format"

-- Parse a blob of text (multiple lines) into an array of log messages
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Given a log message and message tree, return a new message tree with the log inserted
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert (LogMessage _ _ _) mt@(Node _ (Unknown _) _) = mt
insert lm@(LogMessage _ ts _) (Node left msg@(LogMessage _ innerTs _) right) =
  if ts <= innerTs
     then Node (insert lm left) msg right
     else Node left msg (insert lm right)

-- Given a list of log messages, return a sorted tree
build :: [LogMessage] -> MessageTree
build msgs = foldr insert Leaf msgs

-- take a message tree and return the sorted log messages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Extract the message from a LogMessage
getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

-- Return true if msg has priority > 50
importantMessage :: LogMessage -> Bool
importantMessage (Unknown _) = False
importantMessage (LogMessage (Error p) _ _) = p >= 50
importantMessage (LogMessage _ _ _) = False

-- take an unsorted list of log messages and return a list of messages
-- corresponding to any errors with severity of 50 or greater
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  let importantMsgs = filter importantMessage msgs
      tree = build importantMsgs
      orderedMsgs = inOrder tree
  in map getMessage orderedMsgs

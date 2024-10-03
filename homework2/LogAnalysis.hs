
-- {-# OPTIONS_GHC-Wall #-}
module LogAnalysis where 
    
import Log as L
import Util as Util

parseMessage :: String -> LogMessage
parseMessage s =
  case s of 
    ('I':_) -> L.LogMessage Info (Util.parseInt timestamp) (unwords content)
    ('W':_) -> L.LogMessage Warning (Util.parseInt timestamp) (unwords content)
    ('E':_) -> L.LogMessage (Error (Util.parseInt e_severity)) (Util.parseInt e_timestamp) (unwords e_content)
    _ -> Unknown s
    where
      (_:timestamp:content) = words s
      (_:e_severity:e_timestamp:e_content) = words s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error severity) timestamp content):xs)
  | severity > 50 = content : whatWentWrong xs
whatWentWrong (_ : xs) = whatWentWrong xs

-- Function to parse the entire log file
parse :: String -> [LogMessage]
parse logContents = map parseMessage (lines logContents)

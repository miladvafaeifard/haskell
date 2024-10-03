
-- {-# OPTIONS_GHC-Wall #-}
module LogAnalysis where 
    
import Log as L

parseInt :: String -> Int
parseInt s = read s :: Int

parseMessage :: String -> LogMessage
parseMessage s =
  case s of 
    ('I':_) -> L.LogMessage Info (parseInt timestamp) (unwords content)
    ('W':_) -> L.LogMessage Warning (parseInt timestamp) (unwords content)
    ('E':_) -> L.LogMessage (Error (parseInt e_severity)) (parseInt e_timestamp) (unwords e_content)
    _ -> Unknown s
    where
      (_:timestamp:content) = words s
      (_:e_severity:e_timestamp:e_content) = words s

-- Function to parse the entire log file
parse :: String -> [LogMessage]
parse logContents = map parseMessage (lines logContents)

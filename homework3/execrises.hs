import Data.List (intercalate)

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = [ everyNth xs n | n <- [1..Prelude.length xs]]
  where
    everyNth xs n = [x | (i, x) <- zip [1..] xs, i `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) 
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []


-- Function to build the histogram as a String
histogram :: [Integer] -> String
histogram xs = let
    counts = countOccurrences xs    -- Get counts for numbers 0 to 9
    maxCount = maximum counts       -- Find the maximum count
    histogramRows = [buildRow counts i | i <- [maxCount, maxCount-1..1]]  -- Build rows from top to bottom
    in intercalate "\n" (histogramRows ++ ["==========", "0123456789", "\n"])
  where
  -- Function to count how many times each number from 0 to 9 appears
  countOccurrences :: [Integer] -> [Int]
  countOccurrences xs = [length (filter (== n) xs) | n <- [0..9]]

  -- Function to build a single row of the histogram
  buildRow :: [Int] -> Int -> String
  buildRow counts level = [if count >= level then '*' else ' ' | count <- counts]

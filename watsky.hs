import Control.Arrow (Arrow (second))

main = do
  words <- readFile "words.txt"
  print (watskyWords (lines words))

getSubstring :: String -> Int -> Int -> String
getSubstring str start end = take (end - start) (drop start str)

watskyWords :: [String] -> [[String]]
watskyWords words =
  let singleLengthWords = filter (\s -> length s == 9) words
   in [ [first, second, third] | first <- singleLengthWords, second <- singleLengthWords, first /= second, getSubstring first 3 6 == getSubstring second 0 3, third <- singleLengthWords, second /= third, first /= third, getSubstring third 0 3 == getSubstring first 6 9, getSubstring third 3 6 == getSubstring second 6 9
      ]

generalWatskyWords :: [String] -> Int -> Int -> [[String]] -> [[String]]
generalWatskyWords words sylLength numWords currentWords =
  [candidate | word <- words, candidate <- extend words sylLength numWords [word]]

extend :: [String] -> Int -> Int -> [String] -> [[String]]
extend words sylLength numWords currentWords
  | length currentWords == numWords = [currentWords]
  | otherwise =
      [currentWords ++ [word] | word <- words, word `notElem` currentWords, checkSyls sylLength numWords currentWords word]

checkSyls :: Int -> Int -> [String] -> String -> Bool
checkSyls sylLength numWords currentWords word = True

-- let singleLengthWords = filter (\s -> length s == sylLength * numWords) words
-- in [ [first, second, third] | first <- singleLengthWords, second <- singleLengthWords, first /= second, getSubstring first 3 6 == getSubstring second 0 3, third <- singleLengthWords, second /= third, first /= third, getSubstring third 0 3 == getSubstring first 6 9, getSubstring third 3 6 == getSubstring second 6 9
--    ]
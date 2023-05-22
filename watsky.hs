import System.Environment
main = do
  args <- getArgs  
  let sylLength = read (head args) :: Int
  let numWords = read (args !! 1) :: Int
  let fileName = args !! 2
  words <- readFile fileName
  print (generalWatskyWords sylLength numWords (lines words))

generalWatskyWords :: Int -> Int -> [String] -> [[String]]
generalWatskyWords sylLength numWords words =
  let lengthWords = filter (\s -> length s == sylLength * numWords) words
   in extend lengthWords sylLength numWords [[word] | word <- lengthWords]

extend :: [String] -> Int -> Int -> [[String]] -> [[String]]
extend words sylLength numWords candidates
  | length (head candidates) == numWords = candidates
  | otherwise =
      let newCans = concatMap (addNext words sylLength numWords) candidates
       in extend words sylLength numWords newCans

addNext :: [String] -> Int -> Int -> [String] -> [[String]]
addNext words sylLength numWords currentWords
  | length currentWords == numWords = [currentWords]
  | otherwise = [currentWords ++ [word] | word <- words, word `notElem` currentWords, checkWord sylLength numWords currentWords word]

checkWord :: Int -> Int -> [String] -> String -> Bool
checkWord sylLength numWords currentWords word =
  let sylToCheck = length currentWords
      getSyl = getSubstring (sylLength * sylToCheck) (sylLength * (sylToCheck + 1))
      syls = map getSyl currentWords
      newSyls = [getSubstring (sylLength * i) (sylLength * (i + 1)) word | i <- [0 .. (sylToCheck - 1)]]
   in syls == newSyls

getSubstring :: Int -> Int -> String -> String
getSubstring start end str = take (end - start) (drop start str)


--watskyWords :: [String] -> [[String]]
--watskyWords words =
--  let singleLengthWords = filter (\s -> length s == 9) words
--   in [ [first, second, third] | first <- singleLengthWords, second <- singleLengthWords, first /= second, getSubstring 3 6 first == getSubstring 0 3 second, third <- singleLengthWords, second /= third, first /= third, getSubstring 0 3 third == getSubstring 6 9 first, getSubstring 3 6 third == getSubstring 6 9 second
--      ]

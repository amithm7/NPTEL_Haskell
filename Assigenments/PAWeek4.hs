import Data.List    -- For sort function in remRunnerUp

{- A function repl :: String -> String that repeats each letter twice. -}

repl :: String -> String
repl [] = []
repl (x:xs) = x:x:(repl xs)


{- A function remDup :: [Int] -> [Int] that removes duplicates from a list of
integers. That is, if any element of the list is repeated many times, only the
first occurrence of the element should be retained and the others discarded. -}

remDup :: [Int] -> [Int]
remDup [] = []
remDup l
  | dup (last l) (init l) = remDup (init l)
  | otherwise = (remDup (init l)) ++ [(last l)]
  where
  dup :: Int -> [Int] -> Bool
  dup x [] = False
  dup x (y:ys)
    | x /= y = dup x ys
    | otherwise = True

{- A function remChamp :: [Int] -> [Int] that removes the first occurrence of
the largest number in the input list and leaves the remaining list undisturbed.
If the input list is empty, it should return the empty list. -}

remChamp :: [Int] -> [Int]
remChamp [] = []
remChamp (x:xs)
  | g x xs = xs
  | otherwise = x:(remChamp xs)
  where
  g :: Int -> [Int] -> Bool
  g x [] = True
  g x (y:ys)
    | x >= y = g x ys
    | otherwise = False

{- A function remRunnerUp :: [Int] -> [Int] that removes the second largest
number in the input list and leaves the remaining list undisturbed. The second
largest number is defined to be the: Second number in the list if the list is
sorted in descending order.  So, in particular, if the largest number appears
twice, the second largest number is the same as the largest number. If the input
list is has less than two elements, the function should return the input list
unchanged. -}

remRunnerUp :: [Int] -> [Int]
remRunnerUp [] = []
remRunnerUp [x] = [x]
-- aux function so that, sort always takes original list l
remRunnerUp l = aux l
  where
  aux (x:xs)
    -- if head matches 2nd largest element, exclude it, return rest of the list
    | x == (sort l !! (length l - 2)) = xs
    | otherwise = x:(aux xs)

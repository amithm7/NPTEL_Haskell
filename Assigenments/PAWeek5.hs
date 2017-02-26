import Data.List  -- For transpose function used in multiply_matrix

{-
1. G H Hardy recounted the following anectode about Srinivasa Ramanujan..
  I remember once going to see him when he was ill at Putney. I had ridden in
  taxi cab number 1729 and remarked that the number seemed to me rather a dull
  one, and that I hoped it was not an unfavorable omen. "No," he replied, "it is
  a very interesting number; it is the smallest number expressible as the sum of
  two cubes in two different ways."
Define a Ramanujan number to be a positive integer that can be expressed as a
sum of two cubes in at least two different ways. The anecdote above says that
1729 is the smallest Ramanujan number: (1729 = 13 + 123 = 93 + 103).
Define a function - ramanujan :: Int -> Int
such that - ramanujan n - is the nth smallest positive number that can be
expressed  as a sum of two cubes in at least two different ways.
Examples:
ramanujan 1 = 1729
ramanujan 4 = 20683
-}

ramanujan :: Int -> Int
ramanujan n = [(cube x + cube y) | x <- [1..], y <- [1..(x-1)], a <- [(y+1)..(x-1)], b <- [(a+1)..(x-1)], (cube x + cube y) == (cube a + cube b)] !! (n-1)
  where
  cube :: Int -> Int
  cube q = q*q*q

{-
2. A two-dimensional matrix can be represented as a list of rows, each row
itself being a list of elements. So in general it is of type [[a]]. Not every
list of lists is a matrix, though. For instance, [[1,2,3], [], [2,4]] is a list
of three lists, each of a different size.

(a) Define a function
is_matrix :: [[a]] -> Bool
that checks if a list of lists is a valid matrix (nonzero number of rows, each
of the same nonzero length).
Examples:
is_matrix [] = False
is_matrix [[],[],[]] = False
is_matrix [[2,3], [4,5], [6,7]] = True
is_matrix [[2,3,4,5,6,7]] = True
-}

is_matrix :: [[a]] -> Bool
is_matrix [] = False    -- Empty matrix
is_matrix [[]] = False  -- Matrix with empty row
is_matrix [x] = True    -- Matrix with one row
is_matrix (x:y:ys)      -- Matrix with 2 rows and more
  | length x == length y = is_matrix (y:ys)
  | otherwise = False

{-
(b) A square matrix is one where the number of rows is equal to the number of
columns. Define a function
is_square_matrix :: [[a]] -> Bool
that checks if a list of lists is a square matrix.
Examples:
is_square_matrix [] = False
is_square_matrix [[],[],[]] = False
is_square_matrix [[1]] = True
is_square_matrix [[1,2,3], [4,5,6], [7,8,9]] = True
-}

is_square_matrix :: [[a]] -> Bool
is_square_matrix m
  | not (is_matrix m) = False           -- If not a matrix
  | length (head m) == length m = True  -- If rows equal to columns
  | otherwise = False

{-
(c) Given two integer matrices m1 and m2, they can be added if the number of
rows and number of columns are the same. Define a function
addable :: [[Int]] -> [[Int]] -> Bool
that checks if two matrices are addable.
Examples:
addable [[1,2],[3,4]] [[1,2,3],[4,5,6]] = False
addable [[1,2],[3,4]] [[5,6],[7,8]] = True
-}

addable :: [[Int]] -> [[Int]] -> Bool
addable a b
  | not ((is_matrix a) && (is_matrix b)) = False  -- If both are not matrix
  | length a /= length b = False                  -- If rows are not equal
  | length (head a) == length (head b) = True     -- If columns are equal
  | otherwise = False

{-
(d) Define a function that adds two matrices if they are addable (and returns
the empty list if they are not addable).
add_matrix :: [[Int]] -> [[Int]] -> [[Int]]
Examples:
add_matrix [[1,2,3,4]] [[5,6,7,8]] = [[6,8,10,12]]
add_matrix [[1,2],[3,4]] [[5,6],[7,8]] = [[6,8],[10,12]]
-}

add_matrix :: [[Int]] -> [[Int]] -> [[Int]]
add_matrix [] [] = []                       -- Base case, adding empty matrix
add_matrix (x:xs) (y:ys)
  | not (addable (x:xs) (y:ys)) = []        -- If not addable
  | otherwise = (add_row x y):(add_matrix xs ys)
  where
  add_row :: [Int] -> [Int] -> [Int]        -- Addition of elements of a row
  add_row [] [] = []
  add_row (a:as) (b:bs) = (a+b):(add_row as bs)

{-
(e) Given two integer matrices m1 and m2, they can be multiplied if the number
of columns in m1 is the same as the number of rows in m2. Define a function
multiplyable :: [[Int]] -> [[Int]] -> Bool
that checks if two matrices are multiplyable.
Examples:
multiplyable [[1,2,3],[4,5,6]] [[1,2],[3,4]] = False
multiplyable [[1,2],[3,4]] [[1,2,3],[4,5,6]] = True
-}

multiplyable :: [[Int]] -> [[Int]] -> Bool
multiplyable a b
  | not ((is_matrix a) && (is_matrix b)) = False  -- If both are not matrix
  | length (head a) == length b = True            -- If c's of a equals r's of b
  | otherwise = False

{-
(f) Define a function that mulitplies two matrices if they are multiplyable (and
returns the empty list if they are not multiplyable).
multiply_matrix :: [[Int]] -> [[Int]] -> [[Int]]
Examples:
multiply_matrix [[1,2],[3,4]] [[1,2,3],[4,5,6]] = [[9,12,15],[19,26,33]]
multiply_matrix [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]
-}

multiply_matrix :: [[Int]] -> [[Int]] -> [[Int]]
multiply_matrix a b
  | not (multiplyable a b) = []           -- If not multipliable
  | otherwise = multiply_t a (transpose b)
  where
  multiply_t :: [[Int]] -> [[Int]] -> [[Int]]   -- Multiply, b is transposed
  multiply_t [] _ = []
  multiply_t (x:xs) (y:ys) = (multiply_row x (y:ys)):(multiply_t xs (y:ys))
    where
    multiply_row :: [Int] -> [[Int]] -> [Int]   -- Multiply rows of a
    multiply_row d [] = []
    multiply_row d (f:fs) = (multiply_ele d f):(multiply_row d fs)
      where
      multiply_ele :: [Int] -> [Int] -> Int     -- Sum of multiplied elements
      multiply_ele r c = foldr (+) 0 (zipWith (*) r c)

{-
  In this assignment you have to submit a standalone Haskell program that can be
  compiled using the ghc compiler.  Your program should read some lines of text
  and print them out in reverse order, with each line individually reversed. The
  input will end with a line containing the letters "EOF".
  For instance, if the input is the following:
    Line 1
    Line 2
    EOF
  your program should output:
    1 eniL
    2 eniL

  Test Case 2:
    Functional programming is an
    elegant, concise and powerful
    programming paradigm.
    EOF
  Output:
    na si gnimmargorp lanoitcnuF
    lufrewop dna esicnoc ,tnagele
    .mgidarap gnimmargorp
-}

import System.Exit
main = do
    line <- getLine
    if line == "EOF"
        then exitSuccess
    else do
        putStrLn (reverse line)
        main

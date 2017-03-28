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
    2 eniL
    1 eniL

  Test Case 2:
    Functional programming is an
    elegant, concise and powerful
    programming paradigm.
    EOF
  Output:
    .mgidarap gnimmargorp
    lufrewop dna esicnoc ,tnagele
    na si gnimmargorp lanoitcnuF
-}

main = g []
  where
  g xs = do
    line <- getLine
    if line == "EOF"
      then putStr (unlines xs)
    else
      g ((reverse line):xs)

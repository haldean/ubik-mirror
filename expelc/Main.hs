module Main where
import qualified Parser as P

testProgs = map unlines [
    [
    ": test ^ Int = + 8 102"
    ],
    [
    ": test",
    "  ^ Int -> Int -> Int",
    "  = \\x y -> + x y",
    "  = \\x y -> - x y"
    ],
    [
    ": test",
    "  ^ String -> Int -> Int",
    "  = \\x y ->",
    "    +",
    "      x y",
    "  = \\x y -> 7"
    ]
  ]

printProg p = do
  putStrLn p
  print $ P.parse p
  putStrLn ""
main = mapM_ printProg testProgs

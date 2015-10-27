module Main where
import qualified Parser as P

testProgs = map unlines [
    [
    ": test ^ Int = + 8 102"
    ],
    [
    ": test",
    "  ^ Int -> Int -> Int",
    "  = \\x, y -> + x y"
    ],
    [
    ": test",
    "  ^ String -> Int -> Int",
    "  = \\x, y ->",
    "    +",
    "      x y"
    ]
  ]

main = mapM_ (print . P.parse) testProgs

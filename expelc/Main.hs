{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T
import qualified Parser as P

testProgs = map T.unlines [
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
    "  = λ x y ->",
    "    +",
    "      x y",
    "  = \\x y -> 7"
    ]
  ]

printProg :: T.Text -> IO ()
printProg p = do
  putStrLn $ T.unpack p
  print $ P.parse p
  putStrLn ""

main = mapM_ printProg testProgs

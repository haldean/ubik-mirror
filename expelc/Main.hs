module Main where
import GHC.IO.Encoding
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
    ],
    [
    ": bad",
    "  ^ Int -> Int -> Int",
    "  ^ X -> a | X a",
    "  = \\x y -> - x y"
    ]
  ]

printProg :: T.Text -> IO ()
printProg p = do
  putStrLn $ T.unpack p
  print $ P.parse p
  putStrLn ""

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  mapM_ printProg testProgs

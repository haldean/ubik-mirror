module Blockify where
  import Data.Char
  import qualified Data.Text as T

  data Block a = Block {
    startLine :: Int,
    endLine :: Int,
    indent :: Int,
    contents :: a,
    children :: [Block a]
   }

  instance Show a => Show (Block a) where
    show b = replicate (indent b) ' ' ++ show (startLine b) ++ " " ++
        show (contents b) ++ " {\n" ++
        T.unpack (T.intercalate (T.pack "\n") (map (T.pack . show) (children b)))
        ++ replicate (indent b) ' ' ++ "} " ++ show (endLine b) ++ "\n"

  mergeSameLevel :: [Block T.Text] -> [Block T.Text]
  mergeSameLevel = foldr
    (\b acc -> if null acc then [b] else let prev = head acc in
      if indent b == indent prev
        then Block {
          startLine = startLine b,
          endLine = endLine prev,
          indent = indent b,
          contents = T.intercalate (T.pack "\n") [contents b, contents prev],
          children = []
          } : tail acc
        else b : acc)
    []

  assignChildren' :: [Block T.Text] -> [Block T.Text]
  assignChildren' (x:xs) =
    x { children = takeWhile isChild xs} : assignChildren' (dropWhile isChild xs)
    where
      isChild = (> indent x) . indent
  assignChildren' [] = []

  assignChildren :: Block T.Text -> Block T.Text
  assignChildren b = let newChildren = assignChildren' (children b) in
    if length newChildren == length (children b)
      then b
      else b { children = map assignChildren newChildren }

  createRoot :: [Block T.Text] -> Block T.Text
  createRoot blocks = Block {
    startLine = 0, endLine = endLine (last blocks), indent = -1,
    contents = T.pack "", children = blocks
  }

  toBlocks :: T.Text -> Block T.Text
  toBlocks t = assignChildren . createRoot . mergeSameLevel $ zipWith lineToBlock [0..] (T.lines t)
    where
      lineToBlock i l =
        let stripped = T.dropWhile isSpace l
        in Block {
          startLine = i,
          endLine = i,
          indent = T.length l - T.length stripped,
          contents = stripped,
          children = []
        }

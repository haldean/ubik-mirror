module Blockify where
  import Data.Char
  import qualified Data.Text as T

  data Block = Block { startLine :: Int
                     , endLine :: Int
                     , indent :: Int
                     , contents :: T.Text
                     } deriving (Show)

  mergeBlocks :: [Block] -> [Block]
  mergeBlocks = foldr
    (\b acc -> if null acc then [b] else let prev = head acc in
      if indent b == indent prev
        then Block {
          startLine = startLine b,
          endLine = endLine prev,
          indent = indent b,
          contents = T.intercalate (T.pack "\n") [contents b, contents prev]
          } : tail acc
        else b : acc)
    []

  toBlocks :: T.Text -> [Block]
  toBlocks t = mergeBlocks $ zipWith lineToBlock [0..] (T.lines t)
    where
      lineToBlock i l =
        let stripped = T.dropWhile isSpace l
        in Block {
          startLine = i,
          endLine = i,
          indent = T.length l - T.length stripped,
          contents = stripped
        }

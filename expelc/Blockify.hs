module Blockify where
  import Data.Char
  import qualified Data.Text as T

  data Block = Block { startLine :: Int
                     , endLine :: Int
                     , indent :: Int
                     , contents :: T.Text
                     } deriving (Show)

  toBlocks :: T.Text -> [Block]
  toBlocks t = zipWith lineToBlock [0..] (T.lines t)
    where
      lineToBlock i l =
        let stripped = T.dropWhile isSpace l
        in Block {
          startLine = i,
          endLine = i,
          indent = T.length l - T.length stripped,
          contents = stripped
        }

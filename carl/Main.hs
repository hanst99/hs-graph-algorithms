module Main
where

import System.IO
import Data.Graph.Matrix
import Data.Graph.Class
import Data.Graph.Drawing
import System.IO(hSetBinaryMode,stdout,stderr, hPutStrLn)
import qualified Data.ByteString as B

main :: IO ()
main = do
  let g = addEdge 0 1 . addEdge 0 2 . addEdge 1 2
        . addEdge 2 5 . addEdge 2 6 . addEdge 5 6
        . addEdge 3 5 . addEdge 3 4 . addEdge 4 5
        . addEdge 6 7 . addEdge 6 8 . addEdge 7 8
        $ newAdjacencyMatrix 9
      dotSource = drawGraphAsDot g
  hSetBinaryMode stdout True
  rendered <- renderDot dotSource
  case rendered of
    Just string -> B.putStr string
    Nothing -> hPutStrLn stderr "Failed to render dot"

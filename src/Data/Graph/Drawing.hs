{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Drawing
where

import Data.Graph.Class
import Data.List(intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString(ByteString)
import Data.Monoid((<>))
import System.IO(Handle, hSetBinaryMode,hClose)
import System.Process
import System.Exit(ExitCode(..))

drawGraphAsDot :: Graph g => g -> ByteString
drawGraphAsDot g = "graph {" <> drawEdgesAsDot g <> "}"

drawEdgesAsDot :: Graph g => g -> ByteString
drawEdgesAsDot g = B.intercalate ";" $ map drawNeighborhood (vertices g) where
  drawNeighborhood v = (B8.pack $ show v) <> " -- " <> "{" <> B.intercalate "," (map (B8.pack . show) . filter (>v) $ neighbors g v) <> "}"


renderDot :: ByteString -> IO (Maybe ByteString)
renderDot dotSource = do
  (Just hStdIn, Just hStdOut, _, hProcess) <- createProcess_ "renderDot" (proc "neato" ["-Tpng"]){std_in = CreatePipe, std_out = CreatePipe}
  hSetBinaryMode hStdIn True
  hSetBinaryMode hStdOut True
  B.hPutStr hStdIn dotSource
  hClose hStdIn
  code <- waitForProcess hProcess
  out <- B.hGetContents hStdOut
  hClose hStdOut
  if code == ExitSuccess
  then return $ Just out
  else return Nothing

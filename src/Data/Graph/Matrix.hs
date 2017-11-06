module Data.Graph.Matrix
where

import Data.Graph.Class
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed((!))
import Data.Maybe(fromMaybe)
import Data.Traversable
import Data.List(intercalate)

data AdjacencyMatrix = AdjacencyMatrix {adjacencyMatrixNVertices :: Int, adjacencyMatrixAdjacencies :: UV.Vector Bool}

emptyAdjacencyMatrix :: AdjacencyMatrix
emptyAdjacencyMatrix = AdjacencyMatrix 0 (UV.empty)

newAdjacencyMatrix :: Int -> AdjacencyMatrix
newAdjacencyMatrix n = AdjacencyMatrix n (UV.replicate (n*n) False)

adjacencyMatrixIndex :: Int -> Int -> AdjacencyMatrix -> Maybe Int
adjacencyMatrixIndex x y m | x < 0 || x >= n || y < 0 || y >= n = Nothing
                           | otherwise = Just $ unsafeAdjacencyMatrixIndex x y n where
                               n = adjacencyMatrixNVertices m

unsafeAdjacencyMatrixIndex :: Int -> Int -> Int -> Int
unsafeAdjacencyMatrixIndex x y n = x*n+y

splitN :: Int -> [a] -> [[a]]
splitN n l = go n l where
  go 0 xs = [] : go n xs
  go n [] = [[]]
  go n (x:xs) = let (xs':xss) = go (n-1) xs
                in (x:xs'):xss

adjacencyMatrixToList :: AdjacencyMatrix -> [[Bool]]
adjacencyMatrixToList m = splitN n xs where
  n = adjacencyMatrixNVertices m
  xs = UV.toList $ adjacencyMatrixAdjacencies m

prettyPrintAdjacencyMatrix :: AdjacencyMatrix -> String
prettyPrintAdjacencyMatrix = intercalate "\n"
                           . map (intercalate " " . map (\b -> if b then "1" else "0"))
                           . adjacencyMatrixToList

instance Graph AdjacencyMatrix where
  vertices m = [0..adjacencyMatrixNVertices m-1]
  adjacent m x y  = fromMaybe (error $ "Invalid vertex coordinates " ++ show (x,y)) . fmap (UV.unsafeIndex mat) $ adjacencyMatrixIndex x y m where
    mat = adjacencyMatrixAdjacencies m

instance MutableGraph AdjacencyMatrix where
  addEdge x y g | x == y = error "Can't add loop"
                | otherwise = fromMaybe (error $ "Invalid vertex coordinates "++show(x,y))
                            . fmap ( (\adj -> g {adjacencyMatrixAdjacencies = adj}) . UV.unsafeUpd (adjacencyMatrixAdjacencies g))
                            . fmap (fmap (\n -> (n, True)))
                            $ traverse (\(x,y) -> adjacencyMatrixIndex x y g) [(x,y),(y,x)]
  newVertex g = (n, g') where
    n = adjacencyMatrixNVertices g
    g' = AdjacencyMatrix (n+1) m'
    m' = UV.fromList . map (\(x,y) -> x<n && y <n && adjacent g x y) $ [(x,y) | x <- [0..n], y <- [0..n]]

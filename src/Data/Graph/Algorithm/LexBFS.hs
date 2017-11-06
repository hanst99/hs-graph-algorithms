module Data.Graph.Algorithm.LexBFS where

import Data.Graph.Class
import Data.List(partition)

pivot :: (a -> Bool) -> [[a]] -> [[a]]
pivot p classes = do
  cls <- classes
  let (l,r) = partition p cls
  case (l,r) of
    ([],r) -> [r]
    (l,[]) -> [l]
    _ -> [l,r]

lexBfs :: Graph g => g -> [Vertex]
lexBfs g = go $ [vertices g] where
  go ((x:xs):xss) = x : go (pivot (adjacent g x) (xs:xss))
  go ([]:xss) = go xss
  go [] = []

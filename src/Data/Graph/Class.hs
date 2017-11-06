module Data.Graph.Class
where

type Vertex = Int

class Graph g where
  vertices :: g -> [Vertex]
  adjacent :: g -> Vertex -> Vertex -> Bool
  neighbors :: g -> Vertex -> [Vertex]
  neighbors g x = [y | y <- vertices g, adjacent g x y]
  edges :: g -> [(Vertex,Vertex)]
  edges g = [(x,y) | x <- vertices g, y <- filter (>x) (neighbors g x), adjacent g x y]

class Graph g => MutableGraph g where
  addEdge :: Vertex -> Vertex -> g -> g
  newVertex :: g -> (Int, g)

newVertex' :: MutableGraph g => g -> g
newVertex' = snd . newVertex

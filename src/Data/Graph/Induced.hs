module Data.Graph.Induced
where

import Data.Graph.Class

import qualified Data.IntSet as S
import Data.IntSet(IntSet)

data Induced g = Induced {inducedParentGraph :: g, subVertices :: IntSet }

instance Graph g => Graph (Induced g) where
  vertices = S.toList . subVertices
  adjacent g x y = mem x && mem y && parentAdj x y where
    mem = flip S.member (subVertices g)
    parentAdj = adjacent (inducedParentGraph g)

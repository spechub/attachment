{- |
Module      :  $Header$
Description :  Tree-based implementation of 'Graph' and 'DynGraph' using Data.Map
Copyright   :  (c) Martin Erwig, Christian Maeder and Uni Bremen 1999-2006
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

Tree-based implementation of 'Graph' and 'DynGraph' using Data.IntMap
instead of Data.Graph.Inductive.Internal.FiniteMap
-}

module Common.Lib.Graph
  ( Gr
  , GrContext (..)
  , convertToMap
  , unsafeConstructGr
  , getPaths
  , getPathsTo
  ) where

import Data.Graph.Inductive.Graph
import qualified Data.IntMap as Map

-- | the graph type constructor
newtype Gr a b = Gr { convertToMap :: Map.IntMap (GrContext a b) }

data GrContext a b = GrContext
    { nodeLabel :: a
    , nodeSuccs :: Map.IntMap [b] } 

unsafeConstructGr :: Map.IntMap (GrContext a b) -> Gr a b
unsafeConstructGr = Gr

instance (Show a,Show b) => Show (Gr a b) where
  show (Gr g) = showGraph g

instance Graph Gr where
  empty = Gr Map.empty
  isEmpty (Gr g) = Map.null g
  match = matchGr
  mkGraph vs es = (insEdges es . insNodes vs) empty
  labNodes = map (\ (v, c) -> (v, nodeLabel c)) . Map.toList . convertToMap
  -- more efficient versions of derived class members
  --
  matchAny g = case Map.keys $ convertToMap g of
    [] -> error "Match Exception, Empty Graph"
    h : _ -> let (Just c, g') = matchGr h g in (c, g')
  noNodes (Gr g) = Map.size g
  nodeRange (Gr g) = case Map.keys g of
    [] -> (0, -1)
    ks@(h : _) -> (h, last ks)
  labEdges =
    concatMap (\ (v, cw) -> map (\ (l, w) -> (v, w, l)) 
              $ mkAdj $ nodeSuccs cw) . Map.toList . convertToMap

instance DynGraph Gr where
  (p, v, l, s) & Gr g = let
    mkMap = Map.fromListWith (++) . map (\ (e, w) -> (w, [e]))
    pm = mkMap p
    loops = Map.intersection pm $ Map.singleton v () 
    g1 = if null p then g else Map.mapWithKey (\ w cw ->
             case Map.lookup w pm of
               Nothing -> cw
               Just es -> cw { nodeSuccs =
                 Map.insert v es $ nodeSuccs cw }) g
    g2 = Map.insert v GrContext
      { nodeLabel = l,
        nodeSuccs = Map.unionWith (++) loops $ mkMap s } g1
    in if Map.member v g then error $ "Node Exception, Node: " ++ show v
       else Gr g2

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

showGraph :: (Show a, Show b) => Map.IntMap (GrContext a b) -> String
showGraph gr = unlines $ map (\ (v, c) ->
                           show v ++ ": " ++ show (nodeLabel c) 
                           ++ " -> " ++ show (Map.toList $ nodeSuccs c))
                $ Map.toList gr

mkAdj :: Map.IntMap [b] -> Adj b
mkAdj = concatMap (\ (w, l) -> map (\ e -> (e, w)) l) . Map.toList

{- here cyclic edges are omitted as predecessors, thus they only count
as outgoing and not as ingoing! Therefore it is enough that only
successors are filtered during deletions. -}
matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr v (Gr g) = case Map.lookup v g of
  Nothing -> (Nothing, Gr g)
  Just c -> let
    m = nodeSuccs c
    s = mkAdj m
    g1 = Map.delete v g
    dmap = Map.map (\ cw -> Map.findWithDefault [] v $ nodeSuccs cw) g1
    p = mkAdj dmap
    g2 = Map.map (\ cw -> cw { nodeSuccs = Map.delete v $ nodeSuccs cw }) g1 
    in (Just (p, v, nodeLabel c, s), Gr g2)

{- | compute the possible cycle free paths from a start node.
     The result paths are given in reverse order! -}
getPaths :: [LEdge b] -> Node -> Gr a b -> [[LEdge b]]
getPaths path src gr = case matchGr src gr of
    (Just (_, _, _, s), ng) -> let
      in concatMap (\ (lbl, tgt) -> let np = (src, tgt, lbl) : path in
             np : getPaths np tgt ng) $ filter ((/= src) . snd) s
    _ -> error "getPaths"

-- | compute the possible cycle free paths from a start node to a target node.
getPathsTo :: [LEdge b] -> Node -> Node -> Gr a b -> [[LEdge b]]
getPathsTo _ src tgt (Gr g) = case Map.lookup src g of
    Just c -> let
      m = Map.delete src $ nodeSuccs c 
      g1 = Map.delete src g
      g2 = Map.map (\ cw -> cw { nodeSuccs = Map.delete src $ nodeSuccs cw })
           g1
      ps = map (\ b -> [(src, tgt, b)]) $ Map.findWithDefault [] tgt m
      in ps ++ concatMap (\ (nxt, bs) -> 
               [ (src, nxt, b) : p 
               | b <- bs, p <- getPathsTo [] nxt tgt (Gr g2)])
                         (Map.toList $ Map.delete tgt m)
    _ -> error "getPathsTo"

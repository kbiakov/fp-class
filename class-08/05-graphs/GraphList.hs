module GraphList (Graph, mkGraph, edgeIn, adjacent, nodes, weight, nodesBounds, edges) where

import AbstractGraph as AS
import Data.List
import Data.Array.IArray
import Data.Ix

newtype Graph a t = Graph (Array a [(a, t)]) deriving (Show)

formGraph digraph range' edgeList = listArray range' $ formList digraph range' edgeList

vertWithAdjV edgeList = foldr (\(b, e, w) acc ->
  | elem b acc = acc
  | otherwise = b : acc
) [] edgeList

intCase (b, e, w) v ac
  | b == v = (e, w) : ac
  | e == v = (b, w) : ac
  | otherwise = ac

formList digraph range' edgeList = case digraph of
  True -> foldr (\ v acc -> if (elem v (vertWithAdjV edgeList)) then (foldr (\(b, e, w) ac -> if b == v then (e, w) : ac else ac )[] edgeList ) : acc else [] : acc ) [] r
  False -> foldr (\ v acc -> (foldr (\vw ac -> intCase vw v ac ) [] edgeList) : acc ) [] r
  where
    r = range range'

instance AbstractGraph Graph where
  mkGraph digraph range' edgeList = Graph $ formGraph digraph range' edgeList
  
  edgeIn (Graph arr) (b, e)
    | (not.null) $ arr ! b = (not.null) $ filter (\(x, w) -> x == e) $ arr ! b
    | otherwise = False
	
  adjacent (Graph arr) v = map (\(x, w) -> x) $ arr ! v
  
  nodes (Graph arr) = indices arr
  
  weight b e (Graph arr) = snd . head $ filter (\(x, w) -> x == e) $ arr ! b
  
  edges (Graph arr) = concat $ zipWith (\v adjV -> map (\(x, w) -> (v, x, w)) adjV) indices' elems'
    where
      elems' = elems arr
      indices' = indices arr

  nodesBounds (Graph arr) = (head indices', last indices')
    where
      indices' = indices arr

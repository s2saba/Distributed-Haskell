module Data.Graph where


data Node a = Node String a | Empty deriving Show
data Graph a = Graph [Node a] [(String, String)] deriving Show

empty :: Graph a
empty = Graph [] []

addNode :: Graph a -> Node a -> Graph a
addNode (Graph nodes edges) node = (Graph (node:nodes) edges)

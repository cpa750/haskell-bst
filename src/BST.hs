module BST (
    BST (Leaf, Node),
    elements,
    empty,
    insert,
    isEmpty,
    lookup,
    remove,
    removeIf
) where

import Data.Eq
import Data.Ord

import Prelude hiding (lookup)

-- Defining the data type
data BST k v = Leaf | Node k v (BST k v) (BST k v) deriving Show 

---------------------------------------------------------------------------------
-- Exported functions 
---------------------------------------------------------------------------------

elements :: (Ord k) => BST k v -> [(k, v)] 
elements Leaf             = []
elements (Node k v l r)   = elements l ++ [(k, v)] ++ elements r

empty :: BST k v
empty = Leaf

insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert key value Leaf                = Node key value Leaf Leaf 
insert key value (Node k v l r)  
                        | key > k    = Node k v l (insert key value r)                        
                        | key < k    = Node k v (insert key value l) r
                        | otherwise  = Node key value l r

isEmpty :: BST k v -> Bool 
isEmpty Leaf = True
isEmpty _    = False


lookup :: (Ord k) =>  k -> BST k v -> Maybe v
lookup _   Leaf                      = Nothing
lookup key (Node k v l r)
                        | key > k    = lookup key r
                        | key < k    = lookup key l
                        | otherwise  = Just v

remove :: (Ord k) => k -> BST k v -> BST k v
remove _   Leaf                       = Leaf
remove key (Node k v l r) 
                        | key < k = Node k v (remove key l) r 
                        | key > k = Node k v l              (remove key r) 
                        | otherwise = removeNode (Node k v l r)

removeIf :: (Ord k) => (k -> Bool) -> BST k v -> BST k v
removeIf _  Leaf                    = Leaf
removeIf p (Node k v l r)
                        | p k       =
                            removeNode (Node k v (removeIf p l) (removeIf p r))
                        | otherwise =
                            Node k v (removeIf p l) (removeIf p r)

---------------------------------------------------------------------------------
-- Helper functions, not exported 
---------------------------------------------------------------------------------

minElement :: (Ord k) => BST k v -> (k, v) 
minElement (Node k v Leaf _) = (k, v) 
minElement (Node k v l    _) = minElement l 

removeNode :: (Ord k) => BST k v -> BST k v
removeNode Leaf = Leaf
removeNode (Node _ _ Leaf Leaf) = Leaf
removeNode (Node _ _ l   Leaf)  = l 
removeNode (Node _ _ Leaf   r)  = r 
removeNode (Node _ _ l      r)  = let element = minElement r 
                                      key     = fst element
                                      value   = snd element
                                      r'     = remove key r 
                                      in Node key value l r'

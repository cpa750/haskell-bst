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
elements (Node k v t1 t2) = elements t1 ++ [(k, v)] ++ elements t2

empty :: BST k v
empty = Leaf

insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert key value Leaf                = Node key value Leaf Leaf 
insert key value (Node k v t1 t2)  
                        | key > k    = Node k v t1 (insert key value t2)                        
                        | key < k    = Node k v (insert key value t1) t2
                        | otherwise  = Node key value t1 t2

isEmpty :: BST k v -> Bool 
isEmpty Leaf = True
isEmpty _    = False


lookup :: (Ord k) =>  k -> BST k v -> Maybe v
lookup _   Leaf                      = Nothing
lookup key (Node k v t1 t2)
                        | key > k    = lookup key t2
                        | key < k    = lookup key t1
                        | otherwise  = Just v

remove :: (Ord k) => k -> BST k v -> BST k v
remove _   Leaf                       = Leaf
remove key (Node k v t1 t2) 
                        | key < k = Node k v (remove key t1) t2 
                        | key > k = Node k v t1              (remove key t2) 
                        | otherwise = removeNode (Node k v t1 t2)

removeIf :: (Ord k) => (k -> Bool) -> BST k v -> BST k v
removeIf _  Leaf                    = Leaf
removeIf p (Node k v t1 t2)
                        | p k       =
                            removeNode (Node k v (removeIf p t1) (removeIf p t2))
                        | otherwise =
                            Node k v (removeIf p t1) (removeIf p t2)

---------------------------------------------------------------------------------
-- Helper functions, not exported 
---------------------------------------------------------------------------------

minElement :: (Ord k) => BST k v -> (k, v) 
minElement (Node k v Leaf _) = (k, v) 
minElement (Node k v t1   _) = minElement t1 

removeNode :: (Ord k) => BST k v -> BST k v
removeNode Leaf = Leaf
removeNode (Node _ _ Leaf Leaf) = Leaf
removeNode (Node _ _ t1   Leaf) = t1  
removeNode (Node _ _ Leaf   t2) = t2
removeNode (Node _ _ t1     t2) = let element = minElement t2
                                      key     = fst element
                                      value   = snd element
                                      t2'     = remove key t2
                                      in Node key value t1 t2'


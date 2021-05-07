module BST where

import Data.Eq
import Data.Ord

import Prelude hiding (lookup)

-- Defining the data type
data BST k v = Leaf | Node k v (BST k v) (BST k v) deriving Show 

isEmpty :: BST k v -> Bool 
isEmpty Leaf = True
isEmpty _    = False

insert :: (Eq k, Ord k) => k -> v -> BST k v -> BST k v
insert key value Leaf               = Node key value Leaf Leaf 
insert key value (Node k v t1 t2)  
                        | key == k  = Node key value t1 t2
                        | key > k   = Node k v t1 (insert key value t2)                        
                        | key < k   = Node k v (insert key value t1) t2


lookup :: (Eq k, Ord k) =>  k -> BST k v -> Maybe v
lookup _   Leaf                    = Nothing
lookup key (Node k v t1 t2)
                        | key == k = Just v
                        | key > k  = lookup key t2
                        | key < k  = lookup key t1

elements :: (Eq k, Ord k) => BST k v -> [(k, v)] 
elements Leaf             = []
elements (Node k v t1 t2) = elements t1 ++ [(k, v)] ++ elements t2

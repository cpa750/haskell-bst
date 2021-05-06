module BST where

import Data.Eq
import Data.Ord

import Prelude hiding (lookup)

-- Defining the data type
data BST k v = Leaf | Node k v (BST k v) (BST k v) deriving Show 

isEmpty :: BST k v -> Bool 
isEmpty Leaf = True
isEmpty _    = False

insert :: (Eq k, Ord k) => BST k v -> k -> v -> BST k v
insert Leaf             key value   = Node key value Leaf Leaf 
insert (Node k v t1 t2) key value
                        | key == k  = Node key value t1 t2
                        | key > k   = insert t2 key value
                        | key < k   = insert t1 key value


lookup :: (Eq k, Ord k) => BST k v -> k -> Maybe v
lookup Leaf             _          = Nothing
lookup (Node k v t1 t2) key
                        | key == k = Just v
                        | key > k  = lookup t2 key
                        | key < k  = lookup t1 key

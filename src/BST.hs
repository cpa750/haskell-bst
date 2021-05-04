module BST where

import Data.Eq
import Data.Ord

-- Defining the data type
data BST k v = Leaf | Node k v (BST k v) (BST k v) deriving Show 

insert :: (Eq k, Ord k) => BST k v -> k -> v -> BST k v
insert Leaf k v = Node k v Leaf Leaf 

isEmpty :: BST k v -> Bool 
isEmpty Leaf = True
isEmpty _    = False

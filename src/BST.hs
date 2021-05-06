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
insert _ _ _ = Leaf 

lookup :: (Eq k, Ord k) => BST k v -> k -> Maybe v
lookup _ _ = Nothing

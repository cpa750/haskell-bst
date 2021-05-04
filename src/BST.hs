module BST where

import Data.Eq
import Data.Ord

-- Defining the data type
data BST k v = Leaf | Node k v (BST k v) (BST k v) 

someFunc :: IO ()
someFunc = putStrLn "someFunc"

insert :: (Eq k, Ord k) => BST k v -> k -> v -> BST k v
insert Leaf k v = Node k v Leaf Leaf 

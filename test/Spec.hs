import Test.Framework

import System.Random
import Test.QuickCheck

import BST  

main :: IO ()
main = putStrLn "Test suite not yet implemented"

---------------------------------------------------------------------------------
-- Defining the generator for BST
---------------------------------------------------------------------------------

-- Defining Arbitrary for the BST Type 
instance (Arbitrary k, Arbitrary v, Eq k, Ord k, Enum k, Bounded k, Random k) => Arbitrary (BST k v) where
    arbitrary = sized $ arbitrarySizeBST minBound maxBound 

-- Generator function for a sized BST
arbitrarySizeBST :: (Arbitrary k, Arbitrary v, Eq k, Ord k, Enum k, Bounded k, Random k) => k -> k -> Int -> Gen (BST k v)
-- A tree with size 0 is just a leaf 
arbitrarySizeBST low high 0 = return Leaf 
arbitrarySizeBST low high n = do
    key     <- choose (low, high) 
    value   <- arbitrary 
    -- Recursively getting trees. Bounding the inputs so the result is a search tree
    -- Using abs to make sure we only get positive tree sizes
    -- Dividing n by 2 so we eventually generate a size 0 tree
    left    <- arbitrarySizeBST low (pred key)  (abs (n `div` 2))
    right   <- arbitrarySizeBST (succ key) high (abs (n `div` 2)) 
    return  $ Node key value left right


---------------------------------------------------------------------------------
-- Testing insertion
---------------------------------------------------------------------------------



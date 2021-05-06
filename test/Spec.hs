{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import System.Random
import Test.QuickCheck
import Test.HUnit

import Prelude hiding (lookup)

import BST  

main :: IO ()
main = htfMain htf_thisModulesTests 

---------------------------------------------------------------------------------
-- Defining the generator for the BST datatype
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
    -- Recursively building the tree.
    -- Bounding the inputs based on the current key
    -- so the result is a search tree.
    -- Using abs to make sure we only get positive tree sizes.
    -- Dividing n by 2 so we eventually generate a size 0 tree.
    left    <- arbitrarySizeBST low (pred key)  (abs (n `div` 2))
    right   <- arbitrarySizeBST (succ key) high (abs (n `div` 2)) 
    return  $ Node key value left right


---------------------------------------------------------------------------------
-- Testing isEmpty 
---------------------------------------------------------------------------------

test_isEmptyWhenEmpty :: Assertion   
test_isEmptyWhenEmpty = do assertEqual True (isEmpty Leaf) 

test_isEmptyWhenNotEmpty :: Assertion 
test_isEmptyWhenNotEmpty = do
    let tree = Node 1 "A" Leaf Leaf in 
        assertEqual False (isEmpty tree)

---------------------------------------------------------------------------------
-- Testing insert and lookup 
---------------------------------------------------------------------------------
test_lookupWhenEmpty :: Assertion 
test_lookupWhenEmpty = do
    let tree = Leaf in
        assertNothing (lookup tree 1 :: Maybe String)

prop_insertAndLookupNotEmpty :: BST Int String -> Int -> String -> Bool
prop_insertAndLookupNotEmpty tree key value =
    let tree' = insert tree key value in
        lookup tree' key == Just value

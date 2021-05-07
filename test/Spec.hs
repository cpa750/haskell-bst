{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import System.Random
import Test.QuickCheck
import Test.HUnit

import Data.List
import Data.Map(Map)
import qualified Data.Map as Map

import Prelude hiding (lookup)

import BST  

main :: IO ()
main = htfMain htf_thisModulesTests 

isValidBST :: (Eq k, Ord k) => BST k v -> k -> k -> Bool
isValidBST Leaf                   _   _   = True
isValidBST (Node key value t1 t2) min max = (key > min) && (key < max) &&
                                            (isValidBST t1 min key) &&
                                            (isValidBST t2 key max)

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
-- Testing empty 
---------------------------------------------------------------------------------

test_emptyIsEmpty :: Assertion
test_emptyIsEmpty = do assertBool (isEmpty empty)

test_emptyHasNoElements :: Assertion
test_emptyHasNoElements = do
    -- Is there a way to do this so a type annotation isn't needed?
    assertEqual True (null ((BST.elements empty) :: [(Int, String)]))

---------------------------------------------------------------------------------
-- Testing insert and lookup 
---------------------------------------------------------------------------------

test_lookupWhenEmpty :: Assertion 
test_lookupWhenEmpty = do
    let tree = Leaf in
        -- Is there a way to do this so a type annotation isn't needed?
        assertNothing (BST.lookup 1 tree :: Maybe String)

prop_insertAndLookupNotEmpty ::  Int -> String -> BST Int String -> Bool
prop_insertAndLookupNotEmpty key value tree =
    let tree' = BST.insert key value tree in
        BST.lookup key tree' == Just value 

prop_isValidAfterInsertions :: Map Int String -> Bool
-- I don't like the fact that I have to guard against an empty map,
-- But I can't figure out a way to restrict the input to a non-empty map.
prop_isValidAfterInsertions map 
                            | Map.null map = True
                            | otherwise = 
                                -- Inserting all the elements of the map into the BST
                                let tree = Map.foldrWithKey BST.insert Leaf map
                                in isValidBST tree minBound maxBound 

---------------------------------------------------------------------------------
-- Testing elements ---------------------------------------------------------------------------------

prop_insertElementsMatchesInputMapList :: Map Int String -> Bool
prop_insertElementsMatchesInputMapList map = 
    let mapList = Map.toList map
        tree    = Map.foldrWithKey BST.insert Leaf map
    in  mapList == (BST.elements tree) 

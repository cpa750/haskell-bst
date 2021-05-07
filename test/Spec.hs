{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import System.Random
import Test.QuickCheck
import Test.HUnit

import qualified Data.List as List
import Data.Map(Map, (!))
import qualified Data.Map as Map
import Data.List.Split

import Prelude hiding (lookup)

import BST  

main :: IO ()
main = htfMain htf_thisModulesTests 

isValidBST :: (Eq k, Ord k) => BST k v -> k -> k -> Bool
isValidBST Leaf                   _   _   = True
isValidBST (Node key value t1 t2) min max = (key > min) && (key < max) &&
                                            (isValidBST t1 min key) &&
                                            (isValidBST t2 key max)

middleElement :: [a] -> a
middleElement list | List.length list > 0 = list !! ((List.length list) `div` 2)

-- Inserts elements from a map (given a list of keys) into a BST
-- recursively from the middle element of the key list
insertIntoBST :: (Ord k) => Map k v -> [k] -> BST k v -> BST k v
-- Lord forgive me for this awful code I've written
insertIntoBST map keys tree
    | Map.null map          = empty
    | List.length keys == 0 = tree
    | List.length keys == 1 = BST.insert (keys !! 0) (map ! (keys !! 0)) tree 
    | List.length keys  > 1 = 
        let mid         = middleElement keys
            tree'       = BST.insert mid (map ! mid) tree
            firstHalf   = List.takeWhile (/= mid) keys  
            -- Must preserve the relative ordering of keys,
            -- therefore calling reverse twice
            secondHalf  = reverse (List.takeWhile (/= mid) (reverse keys))
            tree''      = insertIntoBST map firstHalf tree'
        in insertIntoBST map secondHalf tree''


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

-- Sanity checking that a Leaf is an empty tree
test_isEmptyWhenEmpty :: Assertion   
test_isEmptyWhenEmpty = do assertEqual True (isEmpty Leaf) 

-- Calling isEmpty on a non-empty tree should be False 
test_isEmptyWhenNotEmpty :: Assertion 
test_isEmptyWhenNotEmpty = do
    let tree = Node 1 "A" Leaf Leaf in 
        assertEqual False (isEmpty tree)

---------------------------------------------------------------------------------
-- Testing empty 
---------------------------------------------------------------------------------

-- An calling isEmpty on empty should be true 
test_emptyIsEmpty :: Assertion
test_emptyIsEmpty = do assertBool (isEmpty empty)

-- An empty tree converted to a list should result in an empty list
test_emptyHasNoElements :: Assertion
test_emptyHasNoElements = do
    -- Is there a way to do this so a type annotation isn't needed?
    assertEqual True (null ((BST.elements empty) :: [(Int, String)]))

---------------------------------------------------------------------------------
-- Testing insert, lookup, and elements 
---------------------------------------------------------------------------------

-- Looking up a key in an empty tree should be Nothing
test_lookupWhenEmpty :: Assertion 
test_lookupWhenEmpty = do
    let tree = empty in
        -- Is there a way to do this so a type annotation isn't needed?
        assertNothing (BST.lookup 1 tree :: Maybe String)

-- Given a BST, a value should be able to be inserted with insert
-- and retrieved with lookup
prop_insertAndLookupNotEmpty ::  Int -> String -> BST Int String -> Bool
prop_insertAndLookupNotEmpty key value tree =
    let tree' = BST.insert key value tree in
        BST.lookup key tree' == Just value 

-- Given a BST, a value replaced with insert should be the result of
-- lookup
prop_replaceAndLookupNotEmpty ::  Int -> String -> String -> BST Int String -> Bool
prop_replaceAndLookupNotEmpty key value1 value2 tree =
    let tree1 = BST.insert key value1 tree
        tree2 = BST.insert key value2 tree1 in
        BST.lookup key tree2 == Just value2 

-- Inserting into a BST should always result in a valid BST
prop_isValidAfterInsertions :: Map Int String -> Bool
-- I don't like the fact that I have to guard against an empty map,
-- But I can't figure out a way to restrict the input to a non-empty map.
prop_isValidAfterInsertions map = 
    -- Inserting all the elements of the map into the BST
    let tree = insertIntoBST map (Map.keys map) empty 
    in isValidBST tree minBound maxBound 

-- Inserting elements of a map into a BST and converting both to lists
-- should always result in equal lists
prop_insertElementsMatchesInputMapList :: Map Int String -> Bool
prop_insertElementsMatchesInputMapList map = 
    let mapList = Map.toList map
        tree    = insertIntoBST map (Map.keys map) empty 
    in  mapList == (BST.elements tree) 

---------------------------------------------------------------------------------
-- Testing remove
---------------------------------------------------------------------------------

-- Removing a nonexistent key should not change the tree
prop_removeNonexistentKeyDoesntChangeTree :: Map Int String -> Int -> Bool
prop_removeNonexistentKeyDoesntChangeTree map key =
    let mapList = Map.toList map
        tree    = remove key (insertIntoBST map (Map.keys map) empty) 
    in mapList == (BST.elements tree)

-- Adding and removing an element to and from a tree should result
-- in equal lists from the original tree and resulting tree 
prop_removedFromTreeAndInputMapAreEqual :: Map Int String -> Int -> String -> Bool
prop_removedFromTreeAndInputMapAreEqual map key value =
    let tree    = insertIntoBST map (Map.keys map) empty 
        tree1   = BST.insert key value tree
        -- Should be the same as the original tree
        tree2   = BST.remove key tree1
        mapList = Map.toList map
    in mapList == (BST.elements tree2)

test_removeChildlessNode :: Assertion
test_removeChildlessNode = do 
        let map     = Map.fromList [(1, "a"), (0, "b"), (2, "c")]
            tree    = insertIntoBST map (Map.keys map) empty 
            tree'   = BST.remove 2 tree in
                assertEqual (BST.elements tree') [(0, "b"), (1, "a")]

-- test_removeNodeWithLeftChild :: Assertion
-- test_removeNodeWithLeftChild = 
--     do
--         let map = Map.fromList [(1, "a"), (0, "b"), (3, "c"), ()]


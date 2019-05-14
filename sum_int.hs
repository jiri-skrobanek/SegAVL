-- This file demonstrates the functionality of SegAVL tree on numeric v and aggregate = summation

import SegmentTree

{-# LANGUAGE ScopedTypeVariables #-}

instance Aggregable Int where
    aggregate Nothing x = x
    aggregate x Nothing = x
    aggregate (Just x) (Just y) = Just (x+y)

instance Aggregable Integer where
    aggregate Nothing x = x
    aggregate x Nothing = x
    aggregate (Just x) (Just y) = Just (x+y)

testdel :: (SegAVL Int Int) -> Int -> SegAVL Int Int
testdel tree key = segAVLDelete tree key

testins :: (SegAVL Int Int) -> Int -> Int -> SegAVL Int Int
testins tree key value = segAVLInsertAndBalance tree key value


testjoin = joinTwo Empty (1 :: Int) (2 :: Int) Empty
testjoin2 = joinTwo Node { key=(-5)::Int, value=3::Int, lsub=Empty, rsub=Empty, height=1::Int, agg= (Just 3) } (1 :: Int) (2 :: Int) Empty

testrange = segAVLRange (segAVLInsertAndBalance (segAVLInsertAndBalance (segAVLInsertAndBalance Empty (1::Int) (2::Int)) (5::Int) (4::Int)) (7::Int) (5::Int)) 0 10

testfromlist = segAVLBuildFromList [(x::Int,x::Int) | x <- [1..10]]

testtolist = segAVLToList testfromlist

testdelfromlist x = segAVLDelete (testfromlist) x

--testBuildFrom = segAVLBuildFromList [(1,1),(2,2)]
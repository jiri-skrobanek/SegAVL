import SegmentTree

{-# LANGUAGE ScopedTypeVariables #-}

type SumInt = Int

class C v where
    this :: v -> v

instance C Int where
    this = id

instance Aggregable Int where
    --aggregate :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
    aggregate Nothing x = x
    aggregate x Nothing = x
    aggregate (Just x) (Just y) = Just (x+y)

instance Aggregable Integer where
    --aggregate :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
    aggregate Nothing x = x
    aggregate x Nothing = x
    aggregate (Just x) (Just y) = Just (x+y)

testdel :: (SegAVL Int Int) -> Int -> SegAVL Int Int
testdel tree key = segAVLDelete tree key

testins :: (SegAVL Int Int) -> Int -> Int -> SegAVL Int Int
testins tree key value = segAVLInsertAndBalance tree key value

--testjoin = 
--testjoin = joinTwo t1 int1 int2 t2

testjoin = joinTwo Empty (1 :: Int) (2 :: Int) Empty
testjoin2 = joinTwo Node { key=(-5)::Int, value=3::Int, lsub=Empty, rsub=Empty, height=1::Int, agg= (Just 3) } (1 :: Int) (2 :: Int) Empty

--testBuildFrom = segAVLBuildFromList [(1,1),(2,2)]
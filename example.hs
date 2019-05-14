import SegmentTree

instance Aggregable Integer where
    aggregate Nothing x = x
    aggregate x Nothing = x
    aggregate (Just x) (Just y) = Just (x+y)

x = segAVLBuildFromList [(x,x) | x <- [1..10]]
y = segSetValue x 5 2 (\x y -> x + y)
z = segAVLRange y 2 8

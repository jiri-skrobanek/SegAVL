class Aggregable v where
    aggregate :: (Maybe v) -> (Maybe v) -> (Maybe v)

data SegAVL k v = Node { key :: k, value :: v, lsub :: SegAVL k v, rsub :: SegAVL k v, height :: Int, agg :: Maybe v } | Empty 
    deriving Show

combine3 :: (Aggregable v) => Maybe v -> v -> Maybe v -> Maybe v
combine3 a b c = aggregate (Just b) $ aggregate a c

getHeight :: SegAVL k v -> Int
getHeight Empty = 0
getHeight tree = height tree

getAgg :: SegAVL k v -> Maybe v
getAgg Empty = Nothing
getAgg tree = agg tree

segAVLBuildFromList :: (Ord k, Aggregable v) => [(k,v)] -> SegAVL k v
segAVLBuildFromList [] = Empty
segAVLBuildFromList list = Node { key = k, value = v, lsub = lt, rsub = rt, height = h, agg = aggr }
    where (l,r,(k,v)) = getHalves list
          lt = segAVLBuildFromList l
          rt = segAVLBuildFromList r
          h = max (getHeight lt) (getHeight rt) + 1
          aggr = combine3 (getAgg lt) v (getAgg rt)

segAVLinsertAndBalance :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> v -> SegAVL k v
segAVLinsertAndBalance Empty k v =  Node { key = k, value = v, lsub = Empty, rsub = Empty, height = 1, agg = Just v }
segAVLinsertAndBalance tree k v
    | key tree < k = balance insertIntoLeft
    | key tree > k = balance insertIntoRight
    | otherwise = let a = combine3 (getAgg $ lsub tree) v (getAgg $ rsub tree) in Node { key = k, value = v, lsub = lsub tree, rsub = rsub tree, height = height tree, agg = a }
    where insertIntoLeft = segAVLinsertAndBalance (lsub tree) k v 
          insertIntoRight = segAVLinsertAndBalance (lsub tree) k v

segAVLRange :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> k -> (Maybe v)
-- Aggregate all values in the closed interval of [min,max]
segAVLRange Empty _ _ = Nothing
segAVLRange tree min max = aggregate left $ aggregate center right
    where center = if min <= key tree && max >= key tree then Just (value tree) else Nothing
          left = if min < key tree then segAVLRange (lsub tree) min max else Nothing
          right = if max > key tree then segAVLRange (rsub tree) min max else Nothing

segAVLFind :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> (Maybe v)
segAVLFind tree k = segAVLRange tree k k

segAVLBalance :: (Ord k, Aggregable v) => SegAVL k v -> SegAVL k v
segAVLBalance Empty = Empty
segAVLBalance tree
    | rh - lh == 2 = if getHeight (rsub (lsub tree)) > getHeight (rsub (rsub tree)) then rotR tree else rotRL tree
    | lh - rh == 2 = if getHeight (lsub (rsub tree)) > getHeight (lsub (lsub tree)) then rotL tree else rotLR tree
    | otherwise = tree
    where lh = getHeight $ lsub tree
          rh = getHeight $ rsub tree

-- Builds a tree from key-value and subtrees
joinTwo :: (Ord k, Aggregable v) => SegAVL k v -> k -> v -> SegAVL k v -> SegAVL k v
joinTwo tree1 k v tree2 = Node { key = k, value = v, lsub = tree1, rsub = tree2, height = max (getHeight tree1) (getHeight tree2) + 1, agg = combine3 (getAgg $ tree1) v (getAgg $ tree2) }

rotR tree = joinTwo (joinTwo (lsub tree) (key tree) (value tree) (lsub $ rsub tree)) (key $ rsub tree) (value $ rsub tree) (rsub $ rsub tree)
rotL tree = joinTwo (joinTwo (rsub tree) (key tree) (value tree) (rsub $ lsub tree)) (key $ lsub tree) (value $ lsub tree) (lsub $ lsub tree)
rotRL tree = rotR $ joinTwo (lsub tree) (key tree) (value tree) (rotL $ rsub tree)
rotLR tree = rotL $ joinTwo (rotR $ lsub tree) (key tree) (value tree) (rsub tree)

balance = id

getHalves [] = undefined
getHalves list = let l = length list in (take (quot (l-1) 2) list, drop (quot (l+1) 2) list, list!!(quot (l-1) 2))

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a
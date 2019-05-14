module SegmentTree where

class Aggregable v where
    aggregate :: (Maybe v) -> (Maybe v) -> (Maybe v)

data SegAVL k v = Node { key :: k, value :: v, lsub :: SegAVL k v, rsub :: SegAVL k v, height :: Int, agg :: Maybe v } | Empty 

instance (Show k, Show v) => Show (SegAVL k v) where
    show Empty = "Empty"
    show tree = "(Node " ++ show (lsub tree) ++ " " ++ show (key tree) ++ ":" ++ show (value tree) ++ " " ++ show (rsub tree) ++ ")"

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

segAVLInsertAndBalance :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> v -> SegAVL k v
segAVLInsertAndBalance Empty k v =  Node { key = k, value = v, lsub = Empty, rsub = Empty, height = 1, agg = Just v }
segAVLInsertAndBalance tree k v
    | key tree > k = segAVLBalance $ joinTwo insertIntoLeft (key tree) (value tree) (rsub tree)
    | key tree < k = segAVLBalance $ joinTwo (lsub tree) (key tree) (value tree) insertIntoRight
    | otherwise = let a = combine3 (getAgg $ lsub tree) v (getAgg $ rsub tree) in Node { key = k, value = v, lsub = lsub tree, rsub = rsub tree, height = height tree, agg = a }
    where insertIntoLeft = segAVLInsertAndBalance (lsub tree) k v 
          insertIntoRight = segAVLInsertAndBalance (rsub tree) k v

segAVLDelete :: (Ord k, Aggregable v) => SegAVL k v -> k -> SegAVL k v
segAVLDelete Empty _ = Empty
segAVLDelete tree k
    | key tree < k = segAVLBalance $ joinTwo (lsub tree) (key tree) (value tree) (segAVLDelete (rsub tree) k)
    | key tree > k = segAVLBalance $ joinTwo (segAVLDelete (lsub tree) k) (key tree) (value tree) (rsub tree)
    | key tree == k && (getHeight $ lsub tree) == 0 && (getHeight $ rsub tree) == 0 = Empty
    | key tree == k && (getHeight $ lsub tree) > (getHeight $ rsub tree) = let (l, maxk, maxv) = deleteMax $ lsub tree in segAVLBalance $ joinTwo l maxk maxv (rsub tree)
    | otherwise = let (r, mink, minv) = deleteMin $ rsub tree in segAVLBalance $ joinTwo (lsub tree) mink minv r

deleteMin :: (Ord k, Aggregable v) => SegAVL k v -> (SegAVL k v, k, v)
deleteMin tree
    | getHeight (lsub tree) == 0 = (rsub tree, key tree, value tree)
    | otherwise = (segAVLBalance Node { key = key tree, value = value tree, lsub = newl, rsub = rsub tree, height = max (getHeight newl) (getHeight $ rsub tree) + 1, agg = combine3 (agg newl) (value tree) (agg $ rsub tree) }, k, v)
    where (newl, k, v) = deleteMin $ lsub tree

deleteMax :: (Ord k, Aggregable v) => SegAVL k v -> (SegAVL k v, k, v)
deleteMax tree
    | getHeight (rsub tree) == 0 = (lsub tree, key tree, value tree)
    | otherwise = (segAVLBalance Node { key = key tree, value = value tree, rsub = newr, lsub = lsub tree, height = max (getHeight newr) (getHeight $ lsub tree) + 1, agg = combine3 (agg newr) (value tree) (agg $ lsub tree) }, k, v)
    where (newr, k, v) = deleteMin $ rsub tree

segAVLRange :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> k -> (Maybe v)
-- Aggregate all values in the closed interval of [min,max]
segAVLRange Empty _ _ = Nothing
segAVLRange tree min max = aggregate left $ aggregate center right
    where center = if min <= key tree && max >= key tree then Just (value tree) else Nothing
          left = if min < key tree then segAVLRange (lsub tree) min max else Nothing
          right = if max > key tree then segAVLRange (rsub tree) min max else Nothing

segAVLFind :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> (Maybe v)
segAVLFind tree k = segAVLRange tree k k

segAVLMember :: (Ord k, Aggregable v) => (SegAVL k v) -> k -> Bool
segAVLMember tree k = case segAVLFind tree k of 
    Nothing -> False
    _ -> True

segAVLBalance :: (Ord k, Aggregable v) => SegAVL k v -> SegAVL k v
--segAVLBalance = id
segAVLBalance Empty = Empty
segAVLBalance tree
    | rh - lh == 2 = if getHeight (lsub $ rsub tree) < getHeight (rsub $ rsub tree) then rotR tree else rotRL tree
    | lh - rh == 2 = if getHeight (lsub $ lsub tree) > getHeight (rsub $ lsub tree) then rotL tree else rotLR tree
    | otherwise = tree
    where lh = getHeight $ lsub tree
          rh = getHeight $ rsub tree

segAVLToList :: (Ord k, Aggregable v) => SegAVL k v -> [(k,v)]
segAVLToList Empty = []
segAVLToList tree = segAVLToList (lsub tree) ++ [(key tree, value tree)] ++ segAVLToList (rsub tree)

segAVLGetMin :: SegAVL k v -> Maybe (k,v)
segAVLGetMin Empty = Nothing
segAVLGetMin tree = case segAVLGetMin $ lsub tree of
    Nothing -> Just (key tree, value tree)
    Just x -> Just x

segAVLGetMax :: SegAVL k v -> Maybe (k,v)
segAVLGetMax Empty = Nothing
segAVLGetMax tree = case segAVLGetMax $ rsub tree of
    Nothing -> Just (key tree, value tree)
    Just x -> Just x

segAVLLowerBound :: (Ord k) => SegAVL k v -> k -> Maybe (k,v)
segAVLLowerBound Empty k = Nothing
segAVLLowerBound tree k
    | key tree < k = segAVLLowerBound (rsub tree) k
    | otherwise = case segAVLLowerBound (lsub tree) k of
        Nothing -> Just (key tree, value tree)
        x -> x

segAVLUpperBound :: (Ord k) => SegAVL k v -> k -> Maybe (k,v)
segAVLUpperBound Empty k = Nothing
segAVLUpperBound tree k
    | key tree >= k = segAVLUpperBound (lsub tree) k
    | otherwise = case segAVLUpperBound (rsub tree) k of
        Nothing -> Just (key tree, value tree)
        x -> x

-- Builds a tree from key-value and subtrees
joinTwo :: (Ord k, Aggregable v) => SegAVL k v -> k -> v -> SegAVL k v -> SegAVL k v
joinTwo tree1 k v tree2 = Node { key = k, value = v, lsub = tree1, rsub = tree2, height = max (getHeight tree1) (getHeight tree2) + 1, agg = combine3 (getAgg $ tree1) v (getAgg $ tree2) }

rotR tree = joinTwo (joinTwo (lsub tree) (key tree) (value tree) (lsub $ rsub tree)) (key $ rsub tree) (value $ rsub tree) (rsub $ rsub tree)

rotL tree = joinTwo (lsub $ lsub tree) (key $ lsub tree) (value $ lsub tree) (joinTwo (rsub $ lsub tree) (key tree) (value tree) (rsub tree))

rotRL tree = rotR $ joinTwo (lsub tree) (key tree) (value tree) (rotL $ rsub tree)

rotLR tree = rotL $ joinTwo (rotR $ lsub tree) (key tree) (value tree) (rsub tree)

getHalves [] = undefined
getHalves list = let l = length list in (take (quot (l-1) 2) list, drop (quot (l+1) 2) list, list!!(quot (l-1) 2))

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a


{-
lookup k l
    | null l = fail "ListLike lookup: Key not found"
    | otherwise = case head l of
                    (k', v) -> if k' == k
                                   then return v
                                   else lookup k (tail l)
-}

data ListT k v = forall full. (Eq k, ListLike full (k, v)) => ListT full

withListT :: forall b k v. ListT k v -> (forall a. ListLike a (k, v) => a -> b) -> b
withListT s f =
    case s of
         ListT l -> f l
--data MapT k v = (Ord k, Eq k) => MapT (Map.Map k v)
--data forall full. ListLike full (k, v) => 
--    ListT k v = ListT full

class EncapIt a tp b c where
    mkEncap :: a -> tp b c

instance (Eq k, ListLike full (k, v)) => EncapIt full ListT k v where
    mkEncap = ListT

instance Ord k => EncapIt (Map.Map k v) Map.Map k v where
    mkEncap = id 
class MapLike ml where
    lookup :: (Eq k, Ord k, Monad m) => k -> ml k v -> m v

l2 :: (Ord k, Monad m, MapLike ml, EncapIt a ml k v) => (a -> ml k v) -> k -> a -> m v
l2 me k l = lookup k (me l)

instance MapLike ListT where
    lookup k l' = withListT l' (worker k)
        where worker k l
                | null l = fail "ListLike lookup: Key not found"
                | otherwise = case head l of
                                (k', v) -> if k' == k
                                              then return v
                                              else worker k (tail l)

instance MapLike Map.Map where
    lookup = Map.lookup

testlist = [(1, "one"), (2, "two"), (3, "three")]
testmap = Map.fromList testlist

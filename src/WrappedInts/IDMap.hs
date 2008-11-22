{-| The "WrappedInts.IDMap" module is a veneer over IntMap for storing maps
    keyed by HasInts; it also provides some extra functions beyond IntMap. It
    is Copyright 2008 by Aetion Technologies LLC and is proprietary and
    company confidential. -}

module WrappedInts.IDMap 
 ({-* IntMap functions -}
  (!), (\\), null, size, member, notMember, lookup, findWithDefault, empty, singleton,
  insert, insertWith, insertWithKey, insertLookupWithKey,
  delete, adjust, adjustWithKey,
  update, updateWithKey, updateLookupWithKey,
  union, unionWith, unionWithKey, unions, unionsWith,
  difference, differenceWith, differenceWithKey,
  intersection, intersectionWith, intersectionWithKey,
  map, mapWithKey, mapAccum, mapAccumWithKey, fold, foldWithKey,
  elems, keys, keysSet, assocs, toList, fromList, fromListWith, fromListWithKey,
  toAscList, fromAscList, fromAscListWith, fromAscListWithKey, fromDistinctAscList,
  filter, filterWithKey, partition, partitionWithKey,
  mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey, split, splitLookup,
  isSubmapOf, isSubmapOfBy, isProperSubmapOf, isProperSubmapOfBy,
  updateMin, updateMax, updateMinWithKey, updateMaxWithKey,
  minView, maxView, minViewWithKey, maxViewWithKey,
  showTree, showTreeWith,
  {-* Useful extras -}
  insertNew, getItemFromMap, getSetFromMap, removeFromMapSet)

where
import Control.Monad (liftM)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Prelude ((.), (++), Eq, Show, Monad, Bool, Int, String, Either, Maybe(..), error, show)
import qualified Prelude (map)
import qualified WrappedInts.IDSet as IDSet (null, delete)
import WrappedInts.Types

-- TODO: Could Template Haskell have done this for us?

-- Helper functions

wrapF :: (HasInt k -> a) -> Int -> a
wrapK1 :: (Int, e) -> (HasInt k, e)
wrapM2 :: (Maybe e, IntMap.IntMap e) -> (Maybe e, HasMap id e)
wrapM2' :: (a, IntMap.IntMap e) -> (a, HasMap id e)
wrapM12 :: (IntMap.IntMap e, IntMap.IntMap e) -> (HasMap k e, HasMap k e)
wrapM12' :: (IntMap.IntMap e1, IntMap.IntMap e2) -> (HasMap k e1, HasMap k e2)
unwrapK1 :: (HasInt k, e) -> (Int, e)
wrapLessScary :: Monad m => m (e, IntMap.IntMap e) -> m (e, HasMap k e)
wrapMoreScary :: Monad m => m ((Int, e), IntMap.IntMap e) -> m ((HasInt k, e), HasMap k e)

wrapF = (. HasInt)
wrapK1  (k,  x)  = (HasInt k,         x)
wrapM2  (x,  m)  = (       x,  HasMap m)
wrapM2' (x,  m)  = (       x,  HasMap m)
wrapM12 (m1, m2) = (HasMap m1, HasMap m2)
wrapM12' (m1, m2) = (HasMap m1, HasMap m2)
unwrapK1 (HasInt k, x) = (k, x)
wrapLessScary = liftM (\(e, m) -> (e, HasMap m))
wrapMoreScary = liftM (\(t, m) -> (wrapK1 t, HasMap m))

-- Wrapping Data.IntMap API

(!) :: HasMap k e -> HasInt k -> e
(\\) :: HasMap k e -> HasMap k e -> HasMap k e
null :: HasMap k e -> Bool
size :: HasMap k e -> Int
member :: HasInt k -> HasMap k e -> Bool
notMember :: HasInt k -> HasMap k e -> Bool
lookup :: Monad m => HasInt k -> HasMap k e -> m e
findWithDefault :: e -> HasInt k -> HasMap k e -> e
empty :: HasMap k e
singleton :: HasInt k -> e -> HasMap k e
insert :: HasInt k -> e -> HasMap k e -> HasMap k e
insertWith :: (e -> e -> e) -> HasInt k -> e -> HasMap k e -> HasMap k e
insertWithKey :: (HasInt k -> e -> e -> e) -> HasInt k -> e -> HasMap k e -> HasMap k e
insertLookupWithKey :: (HasInt k -> e -> e -> e) -> HasInt k -> e -> HasMap k e -> (Maybe e, HasMap k e)
delete :: HasInt k -> HasMap k e -> HasMap k e
adjust :: (e -> e) -> HasInt k -> HasMap k e -> HasMap k e
adjustWithKey :: (HasInt k -> e -> e) -> HasInt k -> HasMap k e -> HasMap k e
update :: (e -> Maybe e) -> HasInt k -> HasMap k e -> HasMap k e
updateWithKey :: (HasInt k -> e -> Maybe e) -> HasInt k -> HasMap k e -> HasMap k e
updateLookupWithKey :: (HasInt k -> e -> Maybe e) -> HasInt k -> HasMap k e -> (Maybe e, HasMap k e)
union :: HasMap k e -> HasMap k e -> HasMap k e
unionWith :: (e -> e -> e) -> HasMap k e -> HasMap k e -> HasMap k e
unionWithKey :: (HasInt k -> e -> e -> e) -> HasMap k e -> HasMap k e -> HasMap k e
unions :: [HasMap k e] -> HasMap k e
unionsWith :: (e -> e -> e) -> [HasMap k e] -> HasMap k e
difference :: HasMap k e1 -> HasMap k e2 -> HasMap k e1
differenceWith :: (e1 -> e2 -> Maybe e1) -> HasMap k e1 -> HasMap k e2 -> HasMap k e1
differenceWithKey :: (HasInt k -> e1 -> e2 -> Maybe e1) -> HasMap k e1 -> HasMap k e2 -> HasMap k e1
intersection :: HasMap k e1 -> HasMap k e2 -> HasMap k e1
intersectionWith :: (e1 -> e2 -> e1) -> HasMap k e1 -> HasMap k e2 -> HasMap k e1
intersectionWithKey :: (HasInt k -> e1 -> e2 -> e1) -> HasMap k e1 -> HasMap k e2 -> HasMap k e1
map :: (e -> e') -> HasMap k e -> HasMap k e'
mapWithKey :: (HasInt k -> e -> e') -> HasMap k e -> HasMap k e'
mapAccum :: (a -> e -> (a, e')) -> a -> HasMap k e -> (a, HasMap k e')
mapAccumWithKey :: (a -> HasInt k -> e -> (a, e')) -> a -> HasMap k e -> (a, HasMap k e')
fold :: (e -> a -> a) -> a -> HasMap k e -> a
foldWithKey :: (HasInt k -> e -> a -> a) -> a -> HasMap k e -> a
elems :: HasMap k e -> [e]
keys :: HasMap k e -> [HasInt k]
keysSet :: HasMap k e -> HasInts k
assocs :: HasMap k e -> [(HasInt k, e)]
toList :: HasMap k e -> [(HasInt k, e)]
fromList :: [(HasInt k, e)] -> HasMap k e
fromListWith :: (e -> e -> e) -> [(HasInt k, e)] -> HasMap k e
fromListWithKey :: (HasInt k -> e -> e -> e) -> [(HasInt k, e)] -> HasMap k e
toAscList :: HasMap k e -> [(HasInt k, e)]
fromAscList :: [(HasInt k, e)] -> HasMap k e
fromAscListWith :: (e -> e -> e) -> [(HasInt k, e)] -> HasMap k e
fromAscListWithKey :: (HasInt k -> e -> e -> e) -> [(HasInt k, e)] -> HasMap k e
fromDistinctAscList :: [(HasInt k, e)] -> HasMap k e
filter :: (e -> Bool) -> HasMap k e -> HasMap k e
filterWithKey :: (HasInt k -> e -> Bool) -> HasMap k e -> HasMap k e
partition :: (e -> Bool) -> HasMap k e -> (HasMap k e, HasMap k e)
partitionWithKey :: (HasInt k -> e -> Bool) -> HasMap k e -> (HasMap k e, HasMap k e)
mapMaybe :: (e -> Maybe e') -> HasMap k e -> HasMap k e'
mapMaybeWithKey :: (HasInt k -> e -> Maybe e') -> HasMap k e -> HasMap k e'
mapEither :: (e -> Either e1 e2) -> HasMap k e -> (HasMap k e1, HasMap k e2)
mapEitherWithKey :: (HasInt k -> e -> Either e1 e2) -> HasMap k e -> (HasMap k e1, HasMap k e2)
split :: HasInt k -> HasMap k e -> (HasMap k e, HasMap k e)
splitLookup :: HasInt k -> HasMap k e -> (HasMap k e, Maybe e, HasMap k e)
isSubmapOf :: Eq e => HasMap k e -> HasMap k e -> Bool
isSubmapOfBy :: (e1 -> e2 -> Bool) -> HasMap k e1 -> HasMap k e2 -> Bool
isProperSubmapOf :: Eq e => HasMap k e -> HasMap k e -> Bool
isProperSubmapOfBy :: (e1 -> e2 -> Bool) -> HasMap k e1 -> HasMap k e2 -> Bool
updateMin :: (e -> e) -> HasMap k e -> HasMap k e
updateMax :: (e -> e) -> HasMap k e -> HasMap k e
updateMinWithKey :: (HasInt k -> e -> e) -> HasMap k e -> HasMap k e
updateMaxWithKey :: (HasInt k -> e -> e) -> HasMap k e -> HasMap k e
minView :: Monad m => HasMap k e -> m (e, HasMap k e)
maxView :: Monad m => HasMap k e -> m (e, HasMap k e)
minViewWithKey :: Monad m => HasMap k e -> m ((HasInt k, e), HasMap k e)
maxViewWithKey :: Monad m => HasMap k e -> m ((HasInt k, e), HasMap k e)
showTree :: Show e => HasMap k e -> String
showTreeWith :: Show e => Bool -> Bool -> HasMap k e -> String

m ! k = wrappedMap m IntMap.! wrappedInt k
m1 \\ m2 = HasMap (wrappedMap m1 IntMap.\\ wrappedMap m2)
null = IntMap.null . wrappedMap
size = IntMap.size . wrappedMap
member k = IntMap.member (wrappedInt k) . wrappedMap
notMember k = IntMap.notMember (wrappedInt k) . wrappedMap
lookup k = IntMap.lookup (wrappedInt k) . wrappedMap
findWithDefault e k = IntMap.findWithDefault e (wrappedInt k) . wrappedMap
empty = HasMap IntMap.empty
singleton k = HasMap . IntMap.singleton (wrappedInt k)
insert k e = HasMap . IntMap.insert (wrappedInt k) e . wrappedMap
insertWith f k e = HasMap . IntMap.insertWith f (wrappedInt k) e . wrappedMap
insertWithKey f k e = HasMap . IntMap.insertWithKey (wrapF f) (wrappedInt k) e . wrappedMap
insertLookupWithKey f k e = wrapM2 . IntMap.insertLookupWithKey (wrapF f) (wrappedInt k) e . wrappedMap
delete k = HasMap . IntMap.delete (wrappedInt k) . wrappedMap
adjust f k = HasMap . IntMap.adjust f (wrappedInt k) . wrappedMap
adjustWithKey f k = HasMap . IntMap.adjustWithKey (wrapF f) (wrappedInt k) . wrappedMap
update f k = HasMap . IntMap.update f (wrappedInt k) . wrappedMap
updateWithKey f k = HasMap . IntMap.updateWithKey (wrapF f) (wrappedInt k) . wrappedMap
updateLookupWithKey f k = wrapM2 . IntMap.updateLookupWithKey (wrapF f) (wrappedInt k) . wrappedMap
union m = HasMap . IntMap.union (wrappedMap m) . wrappedMap
unionWith f m = HasMap . IntMap.unionWith f (wrappedMap m) . wrappedMap
unionWithKey f m = HasMap . IntMap.unionWithKey (wrapF f) (wrappedMap m) . wrappedMap
unions = HasMap . IntMap.unions . Prelude.map wrappedMap
unionsWith f = HasMap . IntMap.unionsWith f . Prelude.map wrappedMap
difference m = HasMap . IntMap.difference (wrappedMap m) . wrappedMap
differenceWith f m = HasMap . IntMap.differenceWith f (wrappedMap m) . wrappedMap
differenceWithKey f m = HasMap . IntMap.differenceWithKey (wrapF f) (wrappedMap m) . wrappedMap
intersection m = HasMap . IntMap.intersection (wrappedMap m) . wrappedMap
intersectionWith f m = HasMap . IntMap.intersectionWith f (wrappedMap m) . wrappedMap
intersectionWithKey f m = HasMap . IntMap.intersectionWithKey (wrapF f) (wrappedMap m) . wrappedMap
map f = HasMap . IntMap.map f . wrappedMap
mapWithKey f = HasMap . IntMap.mapWithKey (wrapF f) . wrappedMap
mapAccum f e = wrapM2' . IntMap.mapAccum f e . wrappedMap
mapAccumWithKey f e = let g e' k = f e' (HasInt k) in wrapM2' . IntMap.mapAccumWithKey g e . wrappedMap
fold f a = IntMap.fold f a . wrappedMap
foldWithKey f a = IntMap.foldWithKey (wrapF f) a . wrappedMap
elems = IntMap.elems . wrappedMap
keys = Prelude.map HasInt . IntMap.keys . wrappedMap
keysSet = HasInts . IntMap.keysSet . wrappedMap
assocs = Prelude.map wrapK1 . IntMap.assocs . wrappedMap
toList = Prelude.map wrapK1 . IntMap.toList . wrappedMap
fromList = HasMap . IntMap.fromList . Prelude.map unwrapK1
fromListWith f = HasMap . IntMap.fromListWith f . Prelude.map unwrapK1
fromListWithKey f = HasMap . IntMap.fromListWithKey (wrapF f) . Prelude.map unwrapK1
toAscList = Prelude.map wrapK1 . IntMap.toAscList . wrappedMap
fromAscList = HasMap . IntMap.fromAscList . Prelude.map unwrapK1
fromAscListWith f = HasMap . IntMap.fromAscListWith f . Prelude.map unwrapK1
fromAscListWithKey f = HasMap . IntMap.fromAscListWithKey (wrapF f) . Prelude.map unwrapK1
fromDistinctAscList = HasMap . IntMap.fromDistinctAscList . Prelude.map unwrapK1
filter p = HasMap . IntMap.filter p . wrappedMap
filterWithKey p = HasMap . IntMap.filterWithKey (wrapF p) . wrappedMap
partition p = wrapM12 . IntMap.partition p . wrappedMap
partitionWithKey p = wrapM12 . IntMap.partitionWithKey (wrapF p) . wrappedMap
mapMaybe f = HasMap . IntMap.mapMaybe f . wrappedMap
mapMaybeWithKey f = HasMap . IntMap.mapMaybeWithKey (wrapF f) . wrappedMap
mapEither f = wrapM12' . IntMap.mapEither f . wrappedMap
mapEitherWithKey f = wrapM12' . IntMap.mapEitherWithKey (wrapF f) . wrappedMap
split k = wrapM12 . IntMap.split (wrappedInt k) . wrappedMap
splitLookup k m = let (m1, e, m2) = IntMap.splitLookup (wrappedInt k) (wrappedMap m) in (HasMap m1, e, HasMap m2)
isSubmapOf m = IntMap.isSubmapOf (wrappedMap m) . wrappedMap
isSubmapOfBy f m = IntMap.isSubmapOfBy f (wrappedMap m) . wrappedMap
isProperSubmapOf m = IntMap.isProperSubmapOf (wrappedMap m) . wrappedMap
isProperSubmapOfBy f m = IntMap.isProperSubmapOfBy f (wrappedMap m) . wrappedMap
updateMin f = HasMap . IntMap.updateMin f . wrappedMap
updateMax f = HasMap . IntMap.updateMax f . wrappedMap
updateMinWithKey f = HasMap . IntMap.updateMinWithKey (wrapF f) . wrappedMap
updateMaxWithKey f = HasMap . IntMap.updateMaxWithKey (wrapF f) . wrappedMap
minView = wrapLessScary . IntMap.minView . wrappedMap
maxView = wrapLessScary . IntMap.maxView . wrappedMap
minViewWithKey = wrapMoreScary . IntMap.minViewWithKey . wrappedMap
maxViewWithKey = wrapMoreScary . IntMap.maxViewWithKey . wrappedMap
showTree = IntMap.showTree . wrappedMap
showTreeWith o1 o2 = IntMap.showTreeWith o1 o2 . wrappedMap

-- Extra functions that are of use to IDMap users

{-| 'insertNew' inserts a key and value into a map that ought not already have
    an element with that key. -}

insertNew :: HasInt k -> e -> HasMap k e -> HasMap k e

insertNew key =
    insertWith err key
    where
      err = error ("multiple instances of " ++ show key)

-- TODO: Consider reversing argument order to match Data.Map conventions

{-| 'getItemFromMap' looks up a key in a map to find the value. It assumes
    that it is in the map. -}

getItemFromMap :: HasMap k a -> HasInt k -> a

getItemFromMap mp key =
    findWithDefault err key mp
    where
      err = error ("failed to find " ++ show key)

{-| 'getSetFromMap' looks up a key in a map to find the values. If the key is
    not in the map, it returns the empty set for the values. -}

getSetFromMap :: Monoid a => HasMap k a -> HasInt k -> a

getSetFromMap mp key =
    findWithDefault mempty key mp

{-| 'removeFromMapSet' removes an element from a wrapped IntSet that is an
    element of a map. If that leaves the IntSet empty, the key is deleted from
    the map. -}

removeFromMapSet :: HasMap k (HasInts a) -> HasInt k -> HasInt a -> HasMap k (HasInts a)

removeFromMapSet mp key element =
    update perhapsNew key mp
    where
      perhapsNew elements =
          let elements' = IDSet.delete element elements
          in if IDSet.null elements' then Nothing else Just elements'

{-| The "WrappedInts.IDSet" module is a veneer over IntSet for storing sets of
    HasInts. It is Copyright 2008 by Aetion Technologies LLC and is
    proprietary and company confidential. -}

module WrappedInts.IDSet 
 ((\\), null, size, member, notMember, isSubsetOf, isProperSubsetOf,
  empty, singleton, insert, delete,
  union, unions, difference, intersection,
  filter, partition, split, splitMember,
  findMin, findMax, deleteMin, deleteMax, deleteFindMin, deleteFindMax, minView, maxView,
  map, fold, elems,
  toList, fromList, toAscList, fromAscList, fromDistinctAscList,
  showTree, showTreeWith)

where
import Control.Monad (liftM)
import qualified Data.IntSet as IntSet
import Prelude ((.), Eq, Show, Monad, Bool, Int, String)
import qualified Prelude (map)
import WrappedInts.Types

-- TODO: Could Template Haskell have done this for us?

-- Helper functions

wrapF :: (HasInt e -> a) -> Int -> a
wrapG :: (HasInt e -> HasInt e') -> Int -> Int
wrapBoth :: (Int, IntSet.IntSet) -> (HasInt e, HasInts e)
wrapS12 :: (IntSet.IntSet, IntSet.IntSet) -> (HasInts e, HasInts e)

wrapF f = f . HasInt
wrapG g = wrappedInt . g . HasInt
wrapBoth (e, s) = (HasInt e, HasInts s)
wrapS12 (s1, s2) = (HasInts s1, HasInts s2)

-- Wrapping Data.IntSet API

(\\) :: HasInts e -> HasInts e -> HasInts e
null :: HasInts e -> Bool
size :: HasInts e -> Int
member :: HasInt e -> HasInts e -> Bool
notMember :: HasInt e -> HasInts e -> Bool
isSubsetOf :: Eq e => HasInts e -> HasInts e -> Bool
isProperSubsetOf :: Eq e => HasInts e -> HasInts e -> Bool
empty :: HasInts e
singleton :: HasInt e -> HasInts e
insert :: HasInt e -> HasInts e -> HasInts e
delete :: HasInt e -> HasInts e -> HasInts e
union :: HasInts e -> HasInts e -> HasInts e
unions :: [HasInts e] -> HasInts e
difference :: HasInts e -> HasInts e -> HasInts e
intersection :: HasInts e -> HasInts e -> HasInts e
filter :: (HasInt e -> Bool) -> HasInts e -> HasInts e
partition :: (HasInt e -> Bool) -> HasInts e -> (HasInts e, HasInts e)
split :: HasInt e -> HasInts e -> (HasInts e, HasInts e)
splitMember :: HasInt e -> HasInts e -> (HasInts e, Bool, HasInts e)
findMin :: HasInts e -> HasInt e
findMax :: HasInts e -> HasInt e
deleteMin :: HasInts e -> HasInts e
deleteMax :: HasInts e -> HasInts e
deleteFindMin :: HasInts e -> (HasInt e, HasInts e)
deleteFindMax :: HasInts e -> (HasInt e, HasInts e)
minView :: Monad m => HasInts e -> m (HasInt e, HasInts e)
maxView :: Monad m => HasInts e -> m (HasInt e, HasInts e)
map :: (HasInt e -> HasInt e') -> HasInts e -> HasInts e'
fold :: (HasInt e -> a -> a) -> a -> HasInts e -> a
elems :: HasInts e -> [HasInt e]
toList :: HasInts e -> [HasInt e]
fromList :: [HasInt e] -> HasInts e
toAscList :: HasInts e -> [HasInt e]
fromAscList :: [HasInt e] -> HasInts e
fromDistinctAscList :: [HasInt e] -> HasInts e
showTree :: Show e => HasInts e -> String
showTreeWith :: Show e => Bool -> Bool -> HasInts e -> String

s1 \\ s2 = HasInts (wrappedInts s1 IntSet.\\ wrappedInts s2)
null = IntSet.null . wrappedInts
size = IntSet.size . wrappedInts
member e = IntSet.member (wrappedInt e) . wrappedInts
notMember e = IntSet.notMember (wrappedInt e) . wrappedInts
isSubsetOf s = IntSet.isSubsetOf (wrappedInts s) . wrappedInts
isProperSubsetOf s = IntSet.isProperSubsetOf (wrappedInts s) . wrappedInts
empty = HasInts IntSet.empty
singleton = HasInts . IntSet.singleton . wrappedInt
insert e = HasInts . IntSet.insert (wrappedInt e) . wrappedInts
delete e = HasInts . IntSet.delete (wrappedInt e) . wrappedInts
union s = HasInts . IntSet.union (wrappedInts s) . wrappedInts
unions = HasInts . IntSet.unions . Prelude.map wrappedInts
difference s = HasInts . IntSet.difference (wrappedInts s) . wrappedInts
intersection s = HasInts . IntSet.intersection (wrappedInts s) . wrappedInts
filter p = HasInts . IntSet.filter (wrapF p) . wrappedInts
partition p = wrapS12 . IntSet.partition (wrapF p) . wrappedInts
split e = wrapS12 . IntSet.split (wrappedInt e) . wrappedInts
splitMember e s = let (s1, p, s2) = IntSet.splitMember (wrappedInt e) (wrappedInts s) in (HasInts s1, p, HasInts s2)
findMin = HasInt . IntSet.findMin . wrappedInts
findMax = HasInt . IntSet.findMax . wrappedInts
deleteMin = HasInts . IntSet.deleteMin . wrappedInts
deleteMax = HasInts . IntSet.deleteMax . wrappedInts
deleteFindMin = wrapBoth . IntSet.deleteFindMin . wrappedInts
deleteFindMax = wrapBoth . IntSet.deleteFindMax . wrappedInts
minView = liftM wrapBoth . IntSet.minView . wrappedInts
maxView = liftM wrapBoth . IntSet.maxView . wrappedInts
map g = HasInts . IntSet.map (wrapG g) . wrappedInts
fold f a = IntSet.fold (wrapF f) a . wrappedInts
elems = Prelude.map HasInt . IntSet.elems . wrappedInts
toList = Prelude.map HasInt . IntSet.toList . wrappedInts
fromList = HasInts . IntSet.fromList . Prelude.map wrappedInt
toAscList = Prelude.map HasInt . IntSet.toAscList . wrappedInts
fromAscList = HasInts . IntSet.fromAscList . Prelude.map wrappedInt
fromDistinctAscList = HasInts . IntSet.fromDistinctAscList . Prelude.map wrappedInt
showTree = IntSet.showTree . wrappedInts
showTreeWith o1 o2 = IntSet.showTreeWith o1 o2 . wrappedInts

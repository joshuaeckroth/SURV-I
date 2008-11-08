{-| The "WrappedInts.Types" module provides the Ints, IntSets and IntMaps
    wrappings that allow the Reasoner to conveniently use phantom types for
    extra type-safety. It is Copyright 2007, 2008 by Aetion Technologies LLC
    and is proprietary and company confidential. -}

module WrappedInts.Types where
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Monoid

{-| 'HasInt' gets the Int in and out of a wrapped Ints. -}

newtype HasInt id = 
    HasInt { wrappedInt :: Int } 

instance Eq (HasInt id) where
    (HasInt x) == (HasInt y) = x == y

instance Ord (HasInt id) where
    compare (HasInt x) (HasInt y) = compare x y

instance Show (HasInt id) where
    show = show . wrappedInt

instance Bounded (HasInt id) where
    minBound = HasInt minBound
    maxBound = HasInt maxBound

instance Enum (HasInt id) where
    succ (HasInt x) = HasInt (succ x)
    pred (HasInt x) = HasInt (pred x)
    toEnum = HasInt
    fromEnum = wrappedInt

instance Num (HasInt id) where
    HasInt x + HasInt y = HasInt (x + y)
    HasInt x - HasInt y = HasInt (x - y)
    HasInt x * HasInt y = HasInt (x * y)
    negate (HasInt x) = HasInt (negate x)
    abs    (HasInt x) = HasInt (abs x)
    signum (HasInt x) = HasInt (signum x)
    fromInteger = HasInt . fromIntegral

instance Real (HasInt id) where
    toRational (HasInt id) = toRational id

instance Integral (HasInt id) where
    quot (HasInt x) (HasInt y) = HasInt (quot x y)
    rem  (HasInt x) (HasInt y) = HasInt (rem x y)
    div  (HasInt x) (HasInt y) = HasInt (div x y)
    mod  (HasInt x) (HasInt y) = HasInt (mod x y)
    quotRem (HasInt x) (HasInt y) = let (x, y) = quotRem x y in (HasInt x, HasInt y)
    divMod  (HasInt x) (HasInt y) = let (x, y) = divMod  x y in (HasInt x, HasInt y)
    toInteger = fromIntegral . wrappedInt

{-| 'HasInts' gets the IntSet in and out of a wrapped IntSet. -}

newtype HasInts id = 
    HasInts { wrappedInts :: IntSet }

instance Eq (HasInts id) where
    (HasInts xs) == (HasInts ys) = xs == ys

instance Ord (HasInts id) where
    compare (HasInts xs) (HasInts ys) = compare xs ys

instance Show (HasInts id) where
    show = show . wrappedInts

instance Monoid (HasInts e) where
    mempty = HasInts mempty
    mappend xs ys = HasInts (mappend (wrappedInts xs) (wrappedInts ys))
    mconcat xss = HasInts (mconcat (map wrappedInts xss))

{-| 'HasMap' gets the IntMap in and out of a wrapped IntMap. -}

newtype HasMap id e = 
    HasMap { wrappedMap :: IntMap e }

instance Eq e => Eq (HasMap id e) where
    (HasMap x) == (HasMap y) = x == y

instance Ord e => Ord (HasMap id e) where
    compare (HasMap x) (HasMap y) = compare x y

instance Show e => Show (HasMap id e) where
    show = show . wrappedMap

instance Monoid (HasMap id e) where
    mempty = HasMap mempty
    mappend xs ys = HasMap (mappend (wrappedMap xs) (wrappedMap ys))
    mconcat xss = HasMap (mconcat (map wrappedMap xss))

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998


-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Provide trees (of instructions), so that lists of instructions can be
-- appended in linear time.
module GHC.Data.OrdList (
        OrdList, pattern NilOL, pattern ConsOL, pattern SnocOL,
        nilOL, isNilOL, unitOL, appOL, consOL, snocOL, concatOL, lastOL,
        headOL,
        mapOL, mapOL', fromOL, toOL, foldrOL, foldlOL,
        partitionOL, reverseOL, fromOLReverse, strictlyEqOL, strictlyOrdOL
) where

import GHC.Prelude
import Data.Foldable

import GHC.Utils.Misc (strictMap)
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semigroup

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

data OrdList a
  = None
  | One a
  | Many (NonEmpty a)
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty
  deriving (Functor)

instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that

instance Semigroup (OrdList a) where
  (<>) = appOL

instance Monoid (OrdList a) where
  mempty = nilOL
  mappend = (Semigroup.<>)
  mconcat = concatOL

instance Foldable OrdList where
  foldr   = foldrOL
  foldl'  = foldlOL
  toList  = fromOL
  null    = isNilOL
  length  = lengthOL

instance Traversable OrdList where
  traverse f xs = toOL <$> traverse f (fromOL xs)

nilOL    :: OrdList a
isNilOL  :: OrdList a -> Bool

unitOL   :: a           -> OrdList a
snocOL   :: OrdList a   -> a         -> OrdList a
consOL   :: a           -> OrdList a -> OrdList a
appOL    :: OrdList a   -> OrdList a -> OrdList a
concatOL :: [OrdList a] -> OrdList a
headOL   :: OrdList a   -> a
lastOL   :: OrdList a   -> a
lengthOL :: OrdList a   -> Int

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
concatOL aas = foldr appOL None aas

pattern NilOL :: OrdList a
pattern NilOL <- (isNilOL -> True) where
  NilOL = None

-- | An unboxed 'Maybe' type with two unboxed fields in the 'Just' case.
-- Useful for defining 'viewCons' and 'viewSnoc' without overhead.
type VMaybe a b = (# (# a, b #) | (# #) #)
pattern VJust :: a -> b -> VMaybe a b
pattern VJust a b = (# (# a, b #) | #)
pattern VNothing :: VMaybe a b
pattern VNothing = (# | (# #) #)
{-# COMPLETE VJust, VNothing #-}

pattern ConsOL :: a -> OrdList a -> OrdList a
pattern ConsOL x xs <- (viewCons -> VJust x xs) where
  ConsOL x xs = consOL x xs
{-# COMPLETE NilOL, ConsOL #-}

viewCons :: OrdList a -> VMaybe a (OrdList a)
viewCons None        = VNothing
viewCons (One a)     = VJust a NilOL
viewCons (Many (a :| [])) = VJust a NilOL
viewCons (Many (a :| b : bs)) = VJust a (Many (b :| bs))
viewCons (Cons a as) = VJust a as
viewCons (Snoc as a) = case viewCons as of
  VJust a' as' -> VJust a' (Snoc as' a)
  VNothing     -> VJust a NilOL
viewCons (Two as1 as2) = case viewCons as1 of
  VJust a' as1' -> VJust a' (Two as1' as2)
  VNothing      -> viewCons as2

pattern SnocOL :: OrdList a -> a -> OrdList a
pattern SnocOL xs x <- (viewSnoc -> VJust xs x) where
  SnocOL xs x = snocOL xs x
{-# COMPLETE NilOL, SnocOL #-}

viewSnoc :: OrdList a -> VMaybe (OrdList a) a
viewSnoc None        = VNothing
viewSnoc (One a)     = VJust NilOL a
viewSnoc (Many as)   = (`VJust` NE.last as) $ case NE.init as of
  [] -> NilOL
  b : bs -> Many (b :| bs)
viewSnoc (Snoc as a) = VJust as a
viewSnoc (Cons a as) = case viewSnoc as of
  VJust as' a' -> VJust (Cons a as') a'
  VNothing     -> VJust NilOL a
viewSnoc (Two as1 as2) = case viewSnoc as2 of
  VJust as2' a' -> VJust (Two as1 as2') a'
  VNothing      -> viewSnoc as1

headOL None        = panic "headOL"
headOL (One a)     = a
headOL (Many as)   = NE.head as
headOL (Cons a _)  = a
headOL (Snoc as _) = headOL as
headOL (Two as _)  = headOL as

lastOL None        = panic "lastOL"
lastOL (One a)     = a
lastOL (Many as)   = NE.last as
lastOL (Cons _ as) = lastOL as
lastOL (Snoc _ a)  = a
lastOL (Two _ as)  = lastOL as

lengthOL None        = 0
lengthOL (One _)     = 1
lengthOL (Many as)   = length as
lengthOL (Cons _ as) = 1 + length as
lengthOL (Snoc as _) = 1 + length as
lengthOL (Two as bs) = length as + length bs

isNilOL None = True
isNilOL _    = False

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

fromOL :: OrdList a -> [a]
fromOL a = go a []
  where go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = a : go b acc
        go (Snoc a b) acc = go a (b:acc)
        go (Two a b)  acc = go a (go b acc)
        go (Many xs)  acc = NE.toList xs ++ acc

fromOLReverse :: OrdList a -> [a]
fromOLReverse a = go a []
        -- acc is already in reverse order
  where go :: OrdList a -> [a] -> [a]
        go None       acc = acc
        go (One a)    acc = a : acc
        go (Cons a b) acc = go b (a : acc)
        go (Snoc a b) acc = b : go a acc
        go (Two a b)  acc = go b (go a acc)
        go (Many xs)  acc = reverse (NE.toList xs) ++ acc

mapOL :: (a -> b) -> OrdList a -> OrdList b
mapOL = fmap

mapOL' :: (a->b) -> OrdList a -> OrdList b
mapOL' _ None        = None
mapOL' f (One x)     = One $! f x
mapOL' f (Cons x xs) = let !x1 = f x
                           !xs1 = mapOL' f xs
                       in Cons x1 xs1
mapOL' f (Snoc xs x) = let !x1 = f x
                           !xs1 = mapOL' f xs
                       in Snoc xs1 x1
mapOL' f (Two b1 b2) = let !b1' = mapOL' f b1
                           !b2' = mapOL' f b2
                       in Two b1' b2'
mapOL' f (Many (x :| xs)) = let !x1 = f x
                                !xs1 = strictMap f xs
                            in Many (x1 :| xs1)

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (One x)     = k x z
foldrOL k z (Cons x xs) = k x (foldrOL k z xs)
foldrOL k z (Snoc xs x) = foldrOL k (k x z) xs
foldrOL k z (Two b1 b2) = foldrOL k (foldrOL k z b2) b1
foldrOL k z (Many xs)   = foldr k z xs

-- | Strict left fold.
foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (One x)     = k z x
foldlOL k z (Cons x xs) = let !z' = (k z x) in foldlOL k z' xs
foldlOL k z (Snoc xs x) = let !z' = (foldlOL k z xs) in k z' x
foldlOL k z (Two b1 b2) = let !z' = (foldlOL k z b1) in foldlOL k z' b2
foldlOL k z (Many xs)   = foldl' k z xs

partitionOL :: (a -> Bool) -> OrdList a -> (OrdList a, OrdList a)
partitionOL _ None = (None,None)
partitionOL f (One x)
  | f x       = (One x, None)
  | otherwise = (None, One x)
partitionOL f (Two xs ys) = (Two ls1 ls2, Two rs1 rs2)
  where !(!ls1,!rs1) = partitionOL f xs
        !(!ls2,!rs2) = partitionOL f ys
partitionOL f (Cons x xs)
  | f x       = (Cons x ls, rs)
  | otherwise = (ls, Cons x rs)
  where !(!ls,!rs) = partitionOL f xs
partitionOL f (Snoc xs x)
  | f x       = (Snoc ls x, rs)
  | otherwise = (ls, Snoc rs x)
  where !(!ls,!rs) = partitionOL f xs
partitionOL f (Many xs) = (toOL ls, toOL rs)
  where !(!ls,!rs) = NE.partition f xs

toOL :: [a] -> OrdList a
toOL [] = None
toOL [x] = One x
toOL (x : xs) = Many (x :| xs)

reverseOL :: OrdList a -> OrdList a
reverseOL None = None
reverseOL (One x) = One x
reverseOL (Cons a b) = Snoc (reverseOL b) a
reverseOL (Snoc a b) = Cons b (reverseOL a)
reverseOL (Two a b)  = Two (reverseOL b) (reverseOL a)
reverseOL (Many xs)  = Many (NE.reverse xs)

-- | Compare not only the values but also the structure of two lists
strictlyEqOL :: Eq a => OrdList a   -> OrdList a -> Bool
strictlyEqOL None         None       = True
strictlyEqOL (One x)     (One y)     = x == y
strictlyEqOL (Cons a as) (Cons b bs) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Snoc as a) (Snoc bs b) = a == b && as `strictlyEqOL` bs
strictlyEqOL (Two a1 a2) (Two b1 b2) = a1 `strictlyEqOL` b1 && a2 `strictlyEqOL` b2
strictlyEqOL (Many as)   (Many bs)   = as == bs
strictlyEqOL _            _          = False

-- | Compare not only the values but also the structure of two lists
strictlyOrdOL :: Ord a => OrdList a   -> OrdList a -> Ordering
strictlyOrdOL None         None       = EQ
strictlyOrdOL None         _          = LT
strictlyOrdOL (One x)     (One y)     = compare x y
strictlyOrdOL (One _)      _          = LT
strictlyOrdOL (Cons a as) (Cons b bs) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Cons _ _)   _          = LT
strictlyOrdOL (Snoc as a) (Snoc bs b) =
  compare a b `mappend` strictlyOrdOL as bs
strictlyOrdOL (Snoc _ _)   _          = LT
strictlyOrdOL (Two a1 a2) (Two b1 b2) =
  (strictlyOrdOL a1 b1) `mappend` (strictlyOrdOL a2 b2)
strictlyOrdOL (Two _ _)    _          = LT
strictlyOrdOL (Many as)   (Many bs)   = compare as bs
strictlyOrdOL (Many _ )   _           = GT

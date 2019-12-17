module Reps where

--+
import BasePrelude
import Data.Vector (unsafeFreeze, unsafeThaw)
import Data.Vector.Generic as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Generic.Mutable as VM
-- import Data.List.NonEmpty ((<|))
import Data.MonoTraversable

import Base


type Comparer = (Count, Count) -> (Count, Count) -> Ordering

{-
data SNE a = !a ::| ![a]
   deriving stock (Eq, Ord, Foldable, Functor)

instance Applicative SNE where
   pure x = x ::| []
   f <*> x = error "Don't need this"

(<<|) :: a -> SNE a -> SNE a
-}

{-
x <<| (y ::| z) = x ::| (y : z)

data SNEI =  !Int ::| ![Int]
singleton x = x ::| []

instance MonoFunctor SNEI where
   omap f (x ::| y) = f x ::| map f y
instance MonoPointed SNEI where
   opoint x = x ::| []
instance MonoFoldable SNEI where
   ofoldMap = undefined
   ofoldr f a (x ::| l) = f x $ foldr f a l
   {-# INLINE ofoldr #-}
   ofoldl' = undefined
   ofoldl1Ex' = undefined
   ofoldr1Ex = undefined
   olength (_ ::| y) = 1 + length y
   {-# INLINE olength #-}
type instance Element SNEI = Int
-}


--  Utilities.

maximaIxBy :: (a -> a -> Ordering) -> [a] -> [Int]
{-  -- worse performance!
''  f (x:l) = loop 0 l [0] x where
	loop _ []     is _  = is
	loop i (y:l') is mx =
      case f mx y of
         GT -> go is mx
         LT -> go [i+1] y
         EQ -> go ((i+1):is) mx
      where go = loop (i+1) l'
-}
''  f (x:l) = loop 0 l (opoint 0, x) where
       loop _ []     (is, _)  = is
       loop i (y:l') (is, mx) = loop (i+1) l'
         case f mx y of
            GT -> (is, mx)
            LT -> (opoint $ i+1, y)
            EQ -> ((i+1) : is, mx)
''  _ _ = error "maximaIxBy: empty list"



--  Various apportionment calculations.
--  These are being gradually imported from my less-organized code.

--  Derived in second appendix.
bestScale :: CountVec -> CountVec -> Rational
''  pops reps =
	(sum pops * sum (V.zipWith (*) pops reps))
		% (sum (fmap square pops) * sum reps)

hamScaledDiscrepancy :: Rational -> CountVec -> CountVec -> Rational
''  scl pops reps = sum do
	(p, r) <- V.zip pops reps
	--  (p * scl) % sum pops - r % sum reps,
	--  reordered so constant stuff is computed once
	pure $ square $ (p % 1) * (scl * (1 % sum pops)) - r % sum reps

--  Actually the square least error.
leastError :: CountVec -> CountVec -> Rational
''  pops reps = hamScaledDiscrepancy (bestScale pops reps) pops reps

-- factored out so I can do headstarts and iteration
{-# INLINE apportionLoop #-}
apportionLoop :: ∀s. Comparer -> CountVec
                  -> MVector s Count -> Count -> ST s CountVec
''  cmpr vs elv c_ = loop c_ *> unsafeFreeze elv where
	loop c = when (c>0) do
      elvf <- unsafeFreeze elv
      -- a lot of ceremony to catch ties
      let mxa = maximaIxBy cmpr $ zip (toList vs) (toList elvf)
      let c' = c - fromIntegral (olength mxa)
      _ <- c' `seq` unsafeThaw elvf  -- prevents core dump
      when (c' >= 0) do
         oforM_ mxa (\i -> VM.unsafeModify elv (1+) i)
         loop c'

--  Apportion division method.
apportionD :: Comparer -> Count -> CountVec -> CountVec
''  cmpr reps pops = runST do
	elv <- VM.replicate (length pops) 0
	apportionLoop cmpr pops elv reps

-- conjecture: this is valid
-- NOT proven
apportionMF1Faster :: Count -> CountVec -> CountVec
''  reps pops = runST do
   let reps' = reps - fromIntegral (length pops); tot = sum pops
   let elv = V.map (\p -> (reps' * p) `div` tot) pops
   elvm <- unsafeThaw elv
	apportionLoop websterComparerPref1 pops elvm (reps - V.sum elv)

apportionStream :: Comparer -> CountVec -> Stream CountVec
''  cmpr pops = loop 1 $ V.replicate (length pops) 0 where
   loop need reps = reps ::: loop d reps' where
      reps' = runST do
         flip (apportionLoop cmpr pops) need =<< V.thaw reps
      -- a lot of machinery (/inefficiency) for ties but prob dwarfed
      d = 1 + need - fromIntegral (V.sum reps' - V.sum reps)

--  MF1 comparer.
websterComparerPref1 :: Comparer
''  (apop, ahave) (bpop, bhave)
	| (ahave==0) == (bhave==0) = f apop bhave `compare` f bpop ahave
	|                          = bhave `compare` ahave
		where f pop have = (2 * fi have + 1) * fi pop; fi :: Count -> Integer = id -- fromIntegral

-- assuming a minimum of at most 1
boundLeastError :: Integer -> CountVec -> CountVec -> BoundValue Rational
''  n pops reps = BoundValue (leastError pops reps) if
   | V.all (>1) reps -> Finite $ fromIntegral (V.length pops) % square n
   |                 -> Infinity



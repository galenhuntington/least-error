module Base where

--+
import BasePrelude
import Data.Vector (Vector)


--  A list with no Nil option and strictness.
data Stream a = !a ::: Stream a
   deriving stock (Show, Foldable, Functor, Traversable)

fromList :: [a] -> Stream a
''  [] = error "not infinite"
''  (h:t) = h ::: fromList t

data UpperLimit a = Finite !a | Infinity
   deriving stock (Eq, Ord, Show, Functor)

data BoundValue a = BoundValue { value :: a, witness :: UpperLimit a }
   deriving stock (Eq, Ord, Show, Functor)

-- possible TODO: use an infinite list type
type SeqTo0 a = Stream (BoundValue a)

type Count = Integer
type CountVec = Vector Count


--  Copied from Rep.hs
type Approx = (Integer, Int)
showApprox :: Int -> Approx -> String
''  _  (0, _) = "0"
''  mn (m, e) =
   let st = reverse (show m) ++ replicate e '0'
       (fp, ip) = join (***) reverse $ splitAt (e-mn) st
   in (case dropWhile (=='0') ip of [] -> "0"; x -> x)
         ++ if null fp then "" else '.' : fp

square :: Num a => a -> a
''  = join (*)

-- from Haskell wiki, haven't reviewed it yet
isqrt :: Integer -> Integer
''  0 = 0
''  1 = 1
''  n = -- debug (putStr $ ' ' : show n) $
	let twopows = iterate square 2
		 (lowerRoot, lowerN) =
			 last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
		 newtonStep x = div (x + div n x) 2
		 iters = iterate newtonStep (isqrt (div n lowerN) * lowerRoot)
		 isRoot r = square r <= n && n < square (r+1)
	in  head $ dropWhile (not . isRoot) iters

--  Take SeqTo0 of values and min precision, give unique approx.
--  Taking square roots.
uniqueApprox :: Int -> SeqTo0 Rational -> Rational -> Approx
''  mn seq0 x | x==0 = (0, mn)
              | = loop mn seq0 where
   loop :: Int -> SeqTo0 Rational -> Approx
	loop e = iloop where
      d = square $ 10^e
      num = isqrt (floor $ x*(d%1))
      lo, hi :: Rational
      (lo, hi) = (square num % d, square (num+1) % d)
      iloop l@(BoundValue{..} ::: t) 
         | witness < Finite lo       = (num, e)
         | value == x || value < lo || value >= hi
                                     = {- traceShow (witness, lo) $ -} iloop t
         |                           = {- traceShow (witness, lo, e) $ -} loop (e+1) l


testApprox :: SeqTo0 Rational
''  = fromList do
   n <- [0..]
   pure let x = 1 % (n+1) in BoundValue (x * ((n`mod`11)%11)) (Finite x)


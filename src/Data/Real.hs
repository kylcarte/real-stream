{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Real where

import Control.Applicative
import Control.Comonad

data Stream a = (:<)
  { head' :: a
  , tail' :: Stream a
  }
infixr 5 :<

instance Functor Stream where
  fmap f (a :< as) = f a :< fmap f as

instance Applicative Stream where
  pure a = a :< pure a
  (f :< fs) <*> (a :< as) = f a :< (fs <*> as)

instance Comonad Stream where
  extract = head'
  extend f as = f as :< extend f (tail' as)
  duplicate as = as :< duplicate (tail' as)

type R = Stream Rational

iterate' :: forall a. (Int -> a -> a) -> a -> Stream a
iterate' f = go 0
  where
  go :: Int -> a -> Stream a
  go n a = a :< go (n+1) (f n a)

generate' :: forall a. (Int -> a) -> Stream a
generate' f = go 0
  where
  go :: Int -> Stream a
  go n = f n :< go (n+1)

push :: [a] -> Stream a -> Stream a
push = \case
  []   -> id
  a:as -> (a :<) . push as

pop :: Int -> Stream a -> [a]
pop n = if n <= 0
  then \_         -> []
  else \(a :< as) -> a : pop (n-1) as

(!) :: Stream a -> Int -> a
(a :< as) ! n = if n <= 0
  then a
  else as ! pred n

drop' :: Int -> Stream a -> Stream a
drop' n = if n <= 0
  then id
  else drop' (n-1) . tail'

cycle' :: forall a. [a] -> Stream a
cycle' as = go as
  where
  go :: [a] -> Stream a
  go = \case
    []      -> go as
    a : as' -> a :< go as'

delay :: Num a => Int -> Stream a -> Stream a
delay n = push $ replicate n 0

zip' :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zip' f = zipW $ \as bs -> f (head' as) (head' bs)

zipW :: (Stream a -> Stream b -> c) -> Stream a -> Stream b -> Stream c
zipW f as bs = f as bs :< zipW f (tail' as) (tail' bs)

instance Num a => Num (Stream a) where
  fromInteger i = fromInteger i :< zero
  (+) = zip' (+)
  (x0 :< x) * (y0 :< y) = undefined
    -- (x0 * y0) :< (x0y + y0x + xy)
    -- where
    -- x0y = (x0 *) <$> y
    -- y0x = (* y0) <$> x
    -- xy  = delay 1 $ x * y
  negate = fmap negate
  abs    = undefined
  signum = undefined

{-

1/(x0 + x1) = 

-}

{-
instance Fractional a => Fractional (Stream a) where
  fromRational r  = fromRational r :< zero
  -- recip (x0 :< x) = recip x0 :< recip x
-}

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f = if n <= 0
  then id
  else f . iterateN (n-1) f

add1 :: Num a => Stream a -> Stream a
add1 = collapse (+)

collapse :: (a -> a -> a) -> Stream a -> Stream a
collapse f (a :< b :< as) = f a b :< as

collapseN :: Int -> (a -> a -> a) -> Stream a -> Stream a
collapseN n = iterateN n . collapse

zero :: Num a => Stream a
zero = 0 :< zero

e_ :: R
e_ = iterate' rec 1
  where
  rec :: Int -> Rational -> Rational
  rec n r = r / toEnum (n+1)

pi_ :: R
pi_ = generate' $ \n -> (4 / ((2 * toEnum n) + 1)) * if even n then 1 else -1

piSqr6 :: R
piSqr6 = generate' $ \n -> if n == 0
  then 0
  else 1 / (toEnum n ^ 2)

approx :: Int -> R -> Double
approx n = fromRational . head' . collapseN n (+)

{-
epsilon :: (Num a, Ord a) => a -> Stream a -> a
-}

truncate' :: Num a => Int -> Stream a -> Stream a
truncate' n (a :< as) = if n <= 0
  then zero
  else a :< truncate' (pred n) as

ss :: Stream Int
ss = 1 :< ((+1) <$> ss)


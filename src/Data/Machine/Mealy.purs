module Data.Machine.Mealy
  ( MealyT
  , runMealyT
  , hoistMealyT
  , Step(..)
  , Source
  , Sink
  , source
  , sink
  , stepMealy
  , runMealy
  , pureMealy
  , mealy
  , halt
  , take
  , drop
  , loop
  , toUnfoldable
  , zipWith
  , scanl
  , collect
  , singleton
  , fromMaybe
  , fromArray
  , msplit
  , interleave
  , when
  , ifte
  , wrapEffect
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Comonad (class Comonad, extract)
import Control.Lazy (class Lazy)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Array ((!!), length)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..), fst, snd, swap)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Effect.Class (class MonadEffect, liftEffect)

newtype MealyT f s a = MealyT (s -> f (Step f s a))

runMealyT :: forall f s a. MealyT f s a -> s -> f (Step f s a)
runMealyT (MealyT f) = f

-- | Transforms a Mealy machine running in the context of `f` into one running 
-- | in `g`, given a natural transformation from `f` to `g`.
hoistMealyT :: forall f g s . Functor g => (f ~> g) -> MealyT f s ~> MealyT g s
hoistMealyT f2g (MealyT goF) = MealyT goG
  where goG s = hoistStep f2g <$> f2g (goF s)

data Step f s a = Emit a (MealyT f s a) | Halt

hoistStep :: forall f g s . Functor g => (f ~> g) -> Step f s ~> Step g s
hoistStep f2g (Emit v nxt) = Emit v (hoistMealyT f2g nxt)
hoistStep _   Halt         = Halt


type Source f s = MealyT f Unit s
type Sink f a = MealyT f a Unit

source :: forall f s. (Monad f) => f s -> Source f s
source src =  mealy $ \_ -> flip Emit (source src) <$> src

sink :: forall f a. (Monad f) => (a -> f Unit) -> Sink f a
sink f = mealy $ \a -> const (Emit unit (sink f)) <$> f a

runMealy :: forall f. (Monad f) => MealyT f Unit Unit -> f Unit
runMealy m = stepMealy unit m >>= f
                where f Halt        = pure unit
                      f (Emit _ m') = runMealy m'

stepMealy :: forall f s a. (Monad f) => s -> MealyT f s a -> f (Step f s a)
stepMealy = flip runMealyT

pureMealy :: forall f s a. (Applicative f) => (s -> Step f s a ) -> MealyT f s a
pureMealy = MealyT <<< map pure

mealy :: forall f s a. (Applicative f) => (s -> f (Step f s a)) -> MealyT f s a
mealy = MealyT

halt :: forall f s a. (Applicative f) => MealyT f s a
halt = pureMealy $ const Halt

take :: forall f s a. (Monad f) => Int -> MealyT f s a -> MealyT f s a
take n m  = if n <= 0 then halt
              else mealy $ \s ->  f <$> stepMealy s m
                                  where f Halt        = Halt
                                        f (Emit a m') = Emit a (take (n - 1) m')

drop :: forall f s a. (Monad f) => Int -> MealyT f s a -> MealyT f s a
drop n m  = if n <= 0 then m
              else mealy $ \s ->  let f Halt        = pure Halt
                                      f (Emit a m') = stepMealy s (drop (n - 1) m')
                                  in  stepMealy s m >>= f

loop :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
loop m0 = loop' m0
  where
  loop' m = mealy $ \s ->
    stepMealy s m >>= case _ of
      Halt -> stepMealy s (loop m0)
      Emit a m' -> pure $ Emit a (loop' m')

toUnfoldable
  :: forall f g s a
   . Unfoldable g
  => Comonad f
  => s -> MealyT f s a -> g a
toUnfoldable s = unfoldr stepUnfold
  where
  stepUnfold m = case extract (runMealyT m s) of
    Emit a m' -> Just $ Tuple a m'
    Halt      -> Nothing

zipWith :: forall f s a b c. (Monad f) => (a -> b -> c) -> MealyT f s a -> MealyT f s b -> MealyT f s c
zipWith f a b = f <$> a <*> b

scanl :: forall f s a b. (Monad f) => (b -> a -> b) -> b -> MealyT f s a -> MealyT f s b
scanl f = go where
    go b m = mealy $ \s ->  let g Halt        = Halt
                                g (Emit a m') = (let b' = f b a in Emit b' (go b' m'))
                             in g <$> stepMealy s m

collect :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (List a)
collect = scanl (flip Cons) Nil

singleton :: forall f s a. (Monad f) => a -> MealyT f s a
singleton a = pureMealy $ \s -> Emit a halt

fromMaybe :: forall f s a. (Monad f) => Maybe a -> MealyT f s a
fromMaybe Nothing  = halt
fromMaybe (Just a) = singleton a

fromArray :: forall f s a. (Monad f) => Array a -> MealyT f s a
fromArray a = let len = length a
                  go n | n < zero || n >= len = halt
                  go n                        = fromMaybe (a !! n) <> go (n + one)
              in  go zero

wrapEffect :: forall f s a. (Monad f) => f a -> MealyT f s a
wrapEffect fa = MealyT $ const (flip Emit halt <$> fa)

-- MonadLogic -- TODO: Create a purescript-logic package
msplit :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (Maybe (Tuple a (MealyT f s a)))
msplit m = mealy $ \s ->  f <$> stepMealy s m
  where f Halt         = Emit (Nothing) halt
        f (Emit a m')  = Emit (Just $ Tuple a m') (msplit m')

interleave :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a -> MealyT f s a
interleave m1 m2 = mealy $ \s ->
  stepMealy s m1 >>= case _ of
    Halt -> stepMealy s m2
    Emit a m1' -> pure $ Emit a (interleave m2 m1')

once :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
once = take 1

ifte :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b -> MealyT f s b
ifte ma f mb = mealy $ \s ->
  stepMealy s ma >>= case _ of
    Halt -> stepMealy s mb
    Emit a ma' -> go ma' <$> stepMealy s (f a)
  where
  go ma' = case _ of
    Halt -> Halt
    Emit b fb -> Emit b (fb <> ifte ma' f mb)

when :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b
when ma f = ifte ma f halt

instance functorMealy :: (Monad f) => Functor (MealyT f s) where
  map f m = mealy $ \s -> g <$> stepMealy s m where
    g (Emit a m') = Emit (f a) (f <$> m')
    g Halt        = Halt

instance applyMealy :: (Monad f) => Apply (MealyT f s) where
  apply f x = mealy $ \s -> ap <$> stepMealy s f <*> stepMealy s x
    where
    ap Halt _ = Halt
    ap _ Halt = Halt
    ap (Emit f' g) (Emit x' y) = Emit (f' x') (g <*> y)

instance applicativeMealy :: (Monad f) => Applicative (MealyT f s) where
  pure t = pureMealy $ \s -> Emit t halt

instance profunctorMealy :: (Monad f) => Profunctor (MealyT f) where
  dimap l r = remap where
    remap m = mealy $ \s -> g <$> stepMealy (l s) m where
                g (Emit c m') = Emit (r c) (remap m')
                g Halt        = Halt

instance strongMealy :: (Monad f) => Strong (MealyT f) where
  first m = mealy $ \s -> let b = fst s
                              d = snd s
                              g (Emit c f') = Emit (Tuple c d) (first f')
                              g Halt        = Halt
                          in  g <$> stepMealy b m
  second = dimap swap swap <<< first

instance semigroupMealy :: (Monad f) => Semigroup (MealyT f s a) where
  append l r = mealy $ \s ->  let g (Emit c l') = pure $ Emit c (l' <> r)
                                  g Halt        = stepMealy s r
                              in  stepMealy s l >>= g

instance monoidMealy :: (Monad f) => Monoid (MealyT f s a) where
  mempty = mealy $ \s -> pure Halt

instance semigroupoidMealy :: (Monad f) => Semigroupoid (MealyT f) where
  compose f g =
    mealy $ \b -> stepMealy b g >>= gb
    where gb Halt = pure Halt
          gb (Emit c g') = fc <$> stepMealy c f
            where
            fc (Emit d f') = Emit d (f' <<< g')
            fc Halt        = Halt

instance categoryMealy :: (Monad f) => Category (MealyT f) where
  identity = pureMealy $ \t -> Emit t halt

instance bindMealy :: (Monad f) => Bind (MealyT f s) where
  bind m f = mealy $ \s -> let g (Emit a m') = h <$> stepMealy s (f a)
                                 where
                                 h (Emit b bs) = Emit b (bs <> (m' >>= f))
                                 h Halt        = Halt
                               g Halt        = pure Halt
                           in  stepMealy s m >>= g

instance monadMealy :: (Monad f) => Monad (MealyT f s)

instance altMealy :: (Monad f) => Alt (MealyT f s) where
  alt x y = mealy $ \s -> let f Halt         = stepMealy s y
                              f (Emit a m')  = pure $ Emit a m'
                          in  stepMealy s x >>= f

instance plusMealy :: (Monad f) => Plus (MealyT f s) where
  empty = halt

instance alternativeMealy :: (Monad f) => Alternative (MealyT f s)

instance monadZero :: (Monad f) => MonadZero (MealyT f s)

instance monadPlus :: (Monad f) => MonadPlus (MealyT f s)

instance monadEffectMealy :: (Monad f, MonadEffect f) => MonadEffect (MealyT f s) where
  liftEffect = wrapEffect <<< liftEffect

instance lazyMealy :: (Monad f) => Lazy (MealyT f s a) where
  defer f = mealy \s -> runMealyT (f unit) s

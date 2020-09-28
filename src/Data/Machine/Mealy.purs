-- | This module provides the building blocks required to create
-- | finite state machines.
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

-- | Mealy is a finite state machine, where:
-- |
-- | - `f` is the effect under which we evaluate,
-- | - `s` is the input state, and
-- | - `a` is the output type.
newtype MealyT f s a = MealyT (s -> f (Step f s a))

runMealyT :: forall f s a. MealyT f s a -> s -> f (Step f s a)
runMealyT (MealyT f) = f

-- | Transforms a Mealy machine running in the context of `f` into one running
-- | in `g`, given a natural transformation from `f` to `g`.
hoistMealyT :: forall f g s . Functor g => (f ~> g) -> MealyT f s ~> MealyT g s
hoistMealyT f2g (MealyT goF) = MealyT goG
  where goG s = hoistStep f2g <$> f2g (goF s)

-- | Step is the core for running machines. Machines can either stop
-- | via the `Halt` constructor, or emit a value and recursively
-- | construct the rest of the machine.
data Step f s a = Emit a (MealyT f s a) | Halt

-- | Transforms a step running in the context of `f` into one running
-- | in `g`, given a natural transformation from `f` to `g`.
hoistStep :: forall f g s . Functor g => (f ~> g) -> Step f s ~> Step g s
hoistStep f2g (Emit v nxt) = Emit v (hoistMealyT f2g nxt)
hoistStep _   Halt         = Halt

-- | Sources are machines with trivial `Unit` input value.
type Source f a = MealyT f Unit a

-- | Sinks are machines with trivial `Unit` output values.
type Sink f s = MealyT f s Unit

-- | Wrap an effectful value into a source. The effect will be repeated
-- | indefinitely.
-- |
-- | For example, generating ten instances of the value 1:
-- | ```purescript
-- | take 10 $ source (pure 1)
-- | ```
source :: forall f s. (Monad f) => f s -> Source f s
source src =  mealy $ \_ -> flip Emit (source src) <$> src

-- | Sinks are 'terminator nodes' in machines. They allow for an
-- | effectful computation to be executed on the inputs.
-- |
-- | For example, logging could be used as a sink:
-- | ```purescript
-- | take 10 $ source (pure 1) >>> sink logShow
-- | ```
sink :: forall f a. (Monad f) => (a -> f Unit) -> Sink f a
sink f = mealy $ \a -> const (Emit unit (sink f)) <$> f a

-- | Run a machine as an effectful computatation.
-- |
-- | For example:
-- | ```purescript
-- | runMealy $ take 10 $ source (pure 1) >>> sink logShow
-- | ```
runMealy :: forall f. (Monad f) => MealyT f Unit Unit -> f Unit
runMealy m = stepMealy unit m >>= f
                where f Halt        = pure unit
                      f (Emit _ m') = runMealy m'

-- | Execute (unroll) a single step on a machine.
stepMealy :: forall f s a. (Monad f) => s -> MealyT f s a -> f (Step f s a)
stepMealy = flip runMealyT

-- | Wrap a pure function into a machine. The function can either
-- | terminate via `Halt`, or `Emit` a value and then decide whether
-- | to `Halt`, continue with a different function, or (usually) wrap
-- | itself via `pureMealy` recursively.
-- |
-- | For example, we can `Halt` on zero:
-- | ```purescript
-- | haltOn0 :: forall f. Applicative f => MealyT f Int Int
-- | haltOn0 = pureMealy go
-- |   where
-- |     go 0 = Halt
-- |     go n = Emit n (pureMealy haltOn0)
-- | ```
pureMealy :: forall f s a. (Applicative f) => (s -> Step f s a ) -> MealyT f s a
pureMealy = MealyT <<< map pure

-- | Wrap an effectful function into a machine. See `pureMealy` for
-- | an example using pure functions.
mealy :: forall f s a. (Applicative f) => (s -> f (Step f s a)) -> MealyT f s a
mealy = MealyT

-- | A machine which halts for any input.
halt :: forall f s a. (Applicative f) => MealyT f s a
halt = pureMealy $ const Halt

-- | Limit the number of outputs of a machine. After using up the `n`
-- | allotted outputs, the machine will halt.
take :: forall f s a. (Monad f) => Int -> MealyT f s a -> MealyT f s a
take n m  = if n <= 0 then halt
              else mealy $ \s ->  f <$> stepMealy s m
                                  where f Halt        = Halt
                                        f (Emit a m') = Emit a (take (n - 1) m')

-- | Skip a number of outputs for a machine.
drop :: forall f s a. (Monad f) => Int -> MealyT f s a -> MealyT f s a
drop n m  = if n <= 0 then m
              else mealy $ \s ->  let f Halt        = pure Halt
                                      f (Emit a m') = stepMealy s (drop (n - 1) m')
                                  in  stepMealy s m >>= f

-- | Loop a machine forever.
loop :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
loop m0 = loop' m0
  where
  loop' m = mealy $ \s ->
    stepMealy s m >>= case _ of
      Halt -> stepMealy s (loop m0)
      Emit a m' -> pure $ Emit a (loop' m')

-- | Extract all the outputs of a machine, given some input.
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

-- | Zip two machines together under some function `f`.
zipWith :: forall f s a b c. (Monad f) => (a -> b -> c) -> MealyT f s a -> MealyT f s b -> MealyT f s c
zipWith f a b = f <$> a <*> b

-- | Accumulate the outputs of a machine into a new machine.
scanl :: forall f s a b. (Monad f) => (b -> a -> b) -> b -> MealyT f s a -> MealyT f s b
scanl f = go where
    go b m = mealy $ \s ->  let g Halt        = Halt
                                g (Emit a m') = (let b' = f b a in Emit b' (go b' m'))
                             in g <$> stepMealy s m

-- | Accumulates the outputs of a machine as a `List`.
collect :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (List a)
collect = scanl (flip Cons) Nil

-- | Creates a machine which emits a single value before halting.
singleton :: forall f s a. (Monad f) => a -> MealyT f s a
singleton a = pureMealy $ \s -> Emit a halt

-- | Creates a machine which either emits a single value before halting
-- | (for `Just`), or just halts (in the case of `Nothing`).
fromMaybe :: forall f s a. (Monad f) => Maybe a -> MealyT f s a
fromMaybe Nothing  = halt
fromMaybe (Just a) = singleton a

-- | Creates a machine whbich emits all the values of the array before
-- | halting.
fromArray :: forall f s a. (Monad f) => Array a -> MealyT f s a
fromArray a = let len = length a
                  go n | n < zero || n >= len = halt
                  go n                        = fromMaybe (a !! n) <> go (n + one)
              in  go zero

-- | Creates a machine which wraps an effectful computation and ignores
-- | its input.
wrapEffect :: forall f s a. (Monad f) => f a -> MealyT f s a
wrapEffect fa = MealyT $ const (flip Emit halt <$> fa)

-- MonadLogic -- TODO: Create a purescript-logic package
-- | Unwrap a machine such that its output is either `Nothign` in case
-- | it would halt, or `Just` the output value and the next computation.
msplit :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (Maybe (Tuple a (MealyT f s a)))
msplit m = mealy $ \s ->  f <$> stepMealy s m
  where f Halt         = Emit (Nothing) halt
        f (Emit a m')  = Emit (Just $ Tuple a m') (msplit m')

-- | Interleaves the values of two machines with matching inputs and
-- | outputs.
interleave :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a -> MealyT f s a
interleave m1 m2 = mealy $ \s ->
  stepMealy s m1 >>= case _ of
    Halt -> stepMealy s m2
    Emit a m1' -> pure $ Emit a (interleave m2 m1')

-- | Takes a single output from a machine.
once :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
once = take 1

-- | If then else: given a machine producing `a`, a continuation `f`,
-- | and a machine producing `b`, generate a machine which will
-- | grab outputs from the first machine and pass them over to the
-- | continuation as long as neither halts.
-- | Once the process halts, the second (`b`) machine is returned.
ifte :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b -> MealyT f s b
ifte ma f mb = mealy $ \s ->
  stepMealy s ma >>= case _ of
    Halt -> stepMealy s mb
    Emit a ma' -> go ma' <$> stepMealy s (f a)
  where
  go ma' = case _ of
    Halt -> Halt
    Emit b fb -> Emit b (fb <> ifte ma' f mb)

-- | Given a machine and a continuation, it will pass outputs from
-- | the machine to the continuation as long as possible until
-- | one of them halts.
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

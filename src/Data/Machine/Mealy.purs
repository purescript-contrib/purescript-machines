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
  , once
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
-- | - `i` is the input type and
-- | - `o` is the output type.
newtype MealyT f i o = MealyT (i -> f (Step f i o))

runMealyT :: forall f i o. MealyT f i o -> i -> f (Step f i o)
runMealyT (MealyT f) = f

-- | Transforms a Mealy machine running in the context of `f` into one running
-- | in `g`, given a natural transformation from `f` to `g`.
hoistMealyT :: forall f g i. Functor g => (f ~> g) -> MealyT f i ~> MealyT g i
hoistMealyT f2g (MealyT goF) = MealyT goG
  where
  goG i = hoistStep f2g <$> f2g (goF i)

-- | Step is the core for running machines. Machines can either stop
-- | via the `Halt` constructor, or emit a value and recursively
-- | construct the rest of the machine.
data Step f i o = Emit o (MealyT f i o) | Halt

-- | Transforms a step running in the context of `f` into one running
-- | in `g`, given a natural transformation from `f` to `g`.
hoistStep :: forall f g i. Functor g => (f ~> g) -> Step f i ~> Step g i
hoistStep f2g (Emit v nxt) = Emit v (hoistMealyT f2g nxt)
hoistStep _ Halt = Halt

-- | Sources are 'initial nodes' in machines. They allow for data
-- | to be generated.
type Source f o = MealyT f Unit o

-- | Sinks are 'terminator nodes' in machines. They allow for an
-- | effectful computation to be executed on the inputs.
type Sink f i = MealyT f i Unit

-- | Wrap an effectful value into a source. The effect will be repeated
-- | indefinitely.
-- |
-- | For example, generating ten instances of the value 1:
-- | ```purescript
-- | take 10 $ source (pure 1)
-- | ```
source :: forall f o. Functor f => f o -> Source f o
source src = mealy $ \_ -> flip Emit (source src) <$> src

-- | Construct a machine which executes an effectful computation on its inputs.
-- |
-- | For example, logging could be used as a sink:
-- | ```purescript
-- | take 10 $ source (pure 1) >>> sink logShow
-- | ```
sink :: forall f i. Functor f => (i -> f Unit) -> Sink f i
sink f = mealy $ \i -> const (Emit unit (sink f)) <$> f i

-- | Run a machine as an effectful computatation.
-- |
-- | For example:
-- | ```purescript
-- | runMealy $ take 10 $ source (pure 1) >>> sink logShow
-- | ```
runMealy :: forall f. Monad f => MealyT f Unit Unit -> f Unit
runMealy m = stepMealy unit m >>= f
  where
  f Halt = pure unit
  f (Emit _ m') = runMealy m'

-- | Execute (unroll) a single step on a machine.
stepMealy :: forall f i o. i -> MealyT f i o -> f (Step f i o)
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
pureMealy :: forall f i o. Applicative f => (i -> Step f i o) -> MealyT f i o
pureMealy = MealyT <<< map pure

-- | Wrap an effectful function into a machine. See `pureMealy` for
-- | an example using pure functions.
mealy :: forall f i o. (i -> f (Step f i o)) -> MealyT f i o
mealy = MealyT

-- | A machine which halts for any input.
halt :: forall f i o. Applicative f => MealyT f i o
halt = pureMealy $ const Halt

-- | Limit the number of outputs of a machine. After using up the `n`
-- | allotted outputs, the machine will halt.
take :: forall f i o. Applicative f => Int -> MealyT f i o -> MealyT f i o
take n m =
  if n <= 0 then halt
  else mealy $ \i -> f <$> stepMealy i m
  where
  f Halt = Halt
  f (Emit o m') = Emit o (take (n - 1) m')

-- | Skip a number of outputs for a machine.
drop :: forall f i o. Monad f => Int -> MealyT f i o -> MealyT f i o
drop n m =
  if n <= 0 then m
  else mealy $ \i -> do
    let
      f Halt = pure Halt
      f (Emit _ m') = stepMealy i (drop (n - 1) m')

    stepMealy i m >>= f

-- | Loop a machine forever.
loop :: forall f i o. Monad f => MealyT f i o -> MealyT f i o
loop m0 = loop' m0
  where
  loop' m = mealy $ \i ->
    stepMealy i m >>= case _ of
      Halt -> stepMealy i (loop m0)
      Emit o m' -> pure $ Emit o (loop' m')

-- | Extract all the outputs of a machine, given some input.
toUnfoldable
  :: forall f g i o
   . Unfoldable g
  => Comonad f
  => i
  -> MealyT f i o
  -> g o
toUnfoldable i = unfoldr stepUnfold
  where
  stepUnfold m = case extract (runMealyT m i) of
    Emit o m' -> Just $ Tuple o m'
    Halt -> Nothing

-- | Zip two machines together under some function `f`.
zipWith :: forall f i a b c. Apply f => (a -> b -> c) -> MealyT f i a -> MealyT f i b -> MealyT f i c
zipWith f a b = f <$> a <*> b

-- | Accumulate the outputs of a machine into a new machine.
scanl :: forall f i a b. Functor f => (b -> a -> b) -> b -> MealyT f i a -> MealyT f i b
scanl f = go
  where
  go b m = mealy $ \i -> do
    let
      g Halt = Halt
      g (Emit o m') = (let b' = f b o in Emit b' (go b' m'))

    g <$> stepMealy i m

-- | Accumulates the outputs of a machine as a `List`.
collect :: forall f i o. Functor f => MealyT f i o -> MealyT f i (List o)
collect = scanl (flip Cons) Nil

-- | Creates a machine which emits a single value before halting.
singleton :: forall f i o. Applicative f => o -> MealyT f i o
singleton o = pureMealy $ \_ -> Emit o halt

-- | Creates a machine which either emits a single value before halting
-- | (for `Just`), or just halts (in the case of `Nothing`).
fromMaybe :: forall f i o. Applicative f => Maybe o -> MealyT f i o
fromMaybe Nothing = halt
fromMaybe (Just o) = singleton o

-- | Creates a machine which emits all the values of the array before
-- | halting.
fromArray :: forall f i o. Monad f => Array o -> MealyT f i o
fromArray o = do
  let
    len = length o
    go n | n < zero || n >= len = halt
    go n = fromMaybe (o !! n) <> go (n + one)

  go zero

-- | Creates a machine which wraps an effectful computation and ignores
-- | its input.
wrapEffect :: forall f i o. Applicative f => f o -> MealyT f i o
wrapEffect fa = MealyT $ const (flip Emit halt <$> fa)

-- MonadLogic -- TODO: Create a purescript-logic package
-- | Unwrap a machine such that its output is either `Nothing` in case
-- | it would halt, or `Just` the output value and the next computation.
msplit :: forall f i o. Applicative f => MealyT f i o -> MealyT f i (Maybe (Tuple o (MealyT f i o)))
msplit m = mealy $ \i -> f <$> stepMealy i m
  where
  f Halt = Emit (Nothing) halt
  f (Emit o m') = Emit (Just $ Tuple o m') (msplit m')

-- | Interleaves the values of two machines with matching inputs and
-- | outputs.
interleave :: forall f i o. Monad f => MealyT f i o -> MealyT f i o -> MealyT f i o
interleave m1 m2 = mealy $ \i ->
  stepMealy i m1 >>= case _ of
    Halt -> stepMealy i m2
    Emit o m1' -> pure $ Emit o (interleave m2 m1')

-- | Takes a single output from a machine.
once :: forall f s a. Applicative f => MealyT f s a -> MealyT f s a
once = take 1

-- | If then else: given a machine producing `a`, a continuation `f`,
-- | and a machine producing `b`, generate a machine which will
-- | grab outputs from the first machine and pass them over to the
-- | continuation as long as neither halts.
-- | Once the process halts, the second (`b`) machine is returned.
ifte :: forall f i a b. Monad f => MealyT f i a -> (a -> MealyT f i b) -> MealyT f i b -> MealyT f i b
ifte ma f mb = mealy $ \i ->
  stepMealy i ma >>= case _ of
    Halt -> stepMealy i mb
    Emit a ma' -> go ma' <$> stepMealy i (f a)
  where
  go ma' = case _ of
    Halt -> Halt
    Emit b fb -> Emit b (fb <> ifte ma' f mb)

-- | Given a machine and a continuation, it will pass outputs from
-- | the machine to the continuation as long as possible until
-- | one of them halts.
when :: forall f i a b. Monad f => MealyT f i a -> (a -> MealyT f i b) -> MealyT f i b
when ma f = ifte ma f halt

instance functorMealy :: Functor f => Functor (MealyT f i) where
  map f m = mealy $ \i -> g <$> stepMealy i m
    where
    g (Emit o m') = Emit (f o) (f <$> m')
    g Halt = Halt

instance applyMealy :: Apply f => Apply (MealyT f i) where
  apply f x = mealy $ \i -> ap <$> stepMealy i f <*> stepMealy i x
    where
    ap Halt _ = Halt
    ap _ Halt = Halt
    ap (Emit f' g) (Emit x' y) = Emit (f' x') (g <*> y)

instance applicativeMealy :: Applicative f => Applicative (MealyT f i) where
  pure t = pureMealy $ \_ -> Emit t halt

instance profunctorMealy :: Functor f => Profunctor (MealyT f) where
  dimap l r = remap
    where
    remap m = mealy $ \i -> g <$> stepMealy (l i) m
      where
      g (Emit c m') = Emit (r c) (remap m')
      g Halt = Halt

instance strongMealy :: Functor f => Strong (MealyT f) where
  first m = mealy $ \s -> do
    let
      b = fst s
      d = snd s
      g (Emit c f') = Emit (Tuple c d) (first f')
      g Halt = Halt

    g <$> stepMealy b m
  second = dimap swap swap <<< first

instance semigroupMealy :: Monad f => Semigroup (MealyT f i o) where
  append l r = mealy $ \i -> do
    let
      g (Emit c l') = pure $ Emit c (l' <> r)
      g Halt = stepMealy i r

    stepMealy i l >>= g

instance monoidMealy :: Monad f => Monoid (MealyT f i o) where
  mempty = mealy $ \_ -> pure Halt

instance semigroupoidMealy :: Monad f => Semigroupoid (MealyT f) where
  compose f g =
    mealy $ \b -> stepMealy b g >>= gb
    where
    gb Halt = pure Halt
    gb (Emit c g') = fc <$> stepMealy c f
      where
      fc (Emit d f') = Emit d (f' <<< g')
      fc Halt = Halt

instance categoryMealy :: Monad f => Category (MealyT f) where
  identity = pureMealy $ \t -> Emit t halt

instance bindMealy :: Monad f => Bind (MealyT f i) where
  bind m f = mealy $ \i -> do
    let
      g (Emit o m') = h <$> stepMealy i (f o)
        where
        h (Emit b bi) = Emit b (bi <> (m' >>= f))
        h Halt = Halt
      g Halt = pure Halt

    stepMealy i m >>= g

instance monadMealy :: Monad f => Monad (MealyT f i)

instance altMealy :: Monad f => Alt (MealyT f i) where
  alt x y = mealy $ \i -> do
    let
      f Halt = stepMealy i y
      f (Emit o m') = pure $ Emit o m'

    stepMealy i x >>= f

instance plusMealy :: Monad f => Plus (MealyT f i) where
  empty = halt

instance alternativeMealy :: Monad f => Alternative (MealyT f i)

instance monadPlus :: Monad f => MonadPlus (MealyT f i)

instance monadEffectMealy :: MonadEffect f => MonadEffect (MealyT f i) where
  liftEffect = wrapEffect <<< liftEffect

instance lazyMealy :: Lazy (MealyT f i o) where
  defer f = mealy \i -> runMealyT (f unit) i

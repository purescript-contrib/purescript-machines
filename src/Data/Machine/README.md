# Module Documentation

## Module Data.Machine.Mealy

### Types

    data MealyT f s a where
      MealyT :: f (s -> f (Step f s a)) -> MealyT f s a

    type Sink f a = MealyT f a Unit

    type Source f s = MealyT f Unit s

    data Step f s a where
      Emit :: a -> MealyT f s a -> Step f s a
      Halt :: Step f s a


### Type Class Instances

    instance altMealy :: (Monad f) => Alt (MealyT f s)

    instance alternativeMealy :: (Monad f) => Alternative (MealyT f s)

    instance applicativeMealy :: (Monad f) => Applicative (MealyT f s)

    instance applyMealy :: (Monad f) => Apply (MealyT f s)

    instance arrowMealy :: (Monad f) => Arrow (MealyT f)

    instance bindMealy :: (Monad f) => Bind (MealyT f s)

    instance categoryMealy :: (Monad f) => Category (MealyT f)

    instance functorMealy :: (Monad f) => Functor (MealyT f s)

    instance monadMealy :: (Monad f) => Monad (MealyT f s)

    instance monadPlus :: (Monad f) => MonadPlus (MealyT f s)

    instance monoidMealy :: (Monad f) => Monoid (MealyT f s a)

    instance plusMealy :: (Monad f) => Plus (MealyT f s)

    instance profunctorMealy :: (Monad f) => Profunctor (MealyT f)

    instance semigroupMealy :: (Monad f) => Semigroup (MealyT f s a)

    instance semigroupoidMealy :: (Monad f) => Semigroupoid (MealyT f)

    instance strongMealy :: (Monad f) => Strong (MealyT f)


### Values

    (>>-) :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b

    collect :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s [a]

    drop :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a

    fromArray :: forall f s a. (Monad f) => [a] -> MealyT f s a

    fromMaybe :: forall f s a. (Monad f) => M.Maybe a -> MealyT f s a

    halt :: forall f s a. (Applicative f) => MealyT f s a

    ifte :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b -> MealyT f s b

    interleave :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a -> MealyT f s a

    loop :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a

    mealy :: forall f s a. (Applicative f) => (s -> f (Step f s a)) -> MealyT f s a

    msplit :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (M.Maybe (Tuple a (MealyT f s a)))

    pureMealy :: forall f s a. (Applicative f) => (s -> Step f s a) -> MealyT f s a

    runMealy :: forall f. (Monad f) => MealyT f Unit Unit -> f Unit

    scanl :: forall f s a b. (Monad f) => (b -> a -> b) -> b -> MealyT f s a -> MealyT f s b

    singleton :: forall f s a. (Monad f) => a -> MealyT f s a

    sink :: forall f a. (Monad f) => (a -> f Unit) -> Sink f a

    source :: forall f s. (Monad f) => f s -> Source f s

    stepMealy :: forall f s a. (Monad f) => s -> MealyT f s a -> f (Step f s a)

    take :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a

    wrapEffect :: forall f s a. (Monad f) => f a -> MealyT f s a

    zipWith :: forall f s a b c. (Monad f) => (a -> b -> c) -> MealyT f s a -> MealyT f s b -> MealyT f s c




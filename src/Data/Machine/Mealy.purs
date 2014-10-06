module Data.Machine.Mealy
  ( MealyT(..) -- FIXME: bug in purescript externs
  , Step(..)
  , Source()
  , Sink()
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
  , zipWith
  , scanl
  , collect
  ) where

  import Data.Tuple
  import Data.Profunctor
  import Data.Foldable
  import Data.Traversable
  import Data.Monoid
  import qualified Data.Array as A
  import Control.Arrow
  import Control.Monad
  import Control.Bind

  data MealyT f s a = MealyT (f (s -> f (Step f s a)))

  data Step f s a = Emit a (MealyT f s a) | Halt

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
  stepMealy s (MealyT f) = join $ f <*> (pure s)

  pureMealy :: forall f s a. (Applicative f) => (s -> Step f s a ) -> MealyT f s a
  pureMealy f = MealyT $ pure (\s -> pure $ f s)

  mealy :: forall f s a. (Applicative f) => (s -> f (Step f s a)) -> MealyT f s a
  mealy f = MealyT $ pure f

  halt :: forall f s a. (Applicative f) => MealyT f s a
  halt = pureMealy $ const Halt

  take :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a
  take n m  = if n <= 0 then halt 
              else mealy $ \s ->  f <$> stepMealy s m
                                  where f Halt        = Halt
                                        f (Emit a m') = Emit a (take (n - 1) m')

  drop :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a
  drop n m  = if n <= 0 then m
              else mealy $ \s ->  let f Halt        = pure Halt
                                      f (Emit a m') = stepMealy s (drop (n - 1) m')
                                  in  stepMealy s m >>= f 

  loop :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
  loop m = loop' m m where
    loop' m0 m = mealy $ \s ->  let f Halt        = stepMealy s m0
                                    f (Emit a m') = pure $ Emit a (loop' m0 m')
                                in  stepMealy s m

  zipWith :: forall f s a b c. (Monad f) => (a -> b -> c) -> MealyT f s a -> MealyT f s b -> MealyT f s c
  zipWith f a b = f <$> a <*> b

  scanl :: forall f s a b. (Monad f) => (b -> a -> b) -> b -> MealyT f s a -> MealyT f s b
  scanl f = loop where
    loop b m = mealy $ \s ->  let g Halt        = Halt
                                  g (Emit a m') = (let b' = f b a in Emit b' (loop b' m'))
                              in g <$> stepMealy s m

  collect :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s [a]
  collect = scanl (flip (:)) []

  instance functorMealy :: (Monad f) => Functor (MealyT f s) where
    (<$>) f m = mealy $ \s -> g <$> stepMealy s m where
      g (Emit a m') = Emit (f a) (f <$> m')
      g Halt = Halt

  instance applyMealy :: (Monad f) => Apply (MealyT f s) where
    (<*>) f x = mealy $ \s -> let ap Halt _ = Halt
                                  ap _ Halt = Halt
                                  ap (Emit f f') (Emit x x') = Emit (f x) (f' <*> x')
                              in  ap <$> stepMealy s f <*> stepMealy s x

  instance applicativeMealy :: (Monad f) => Applicative (MealyT f s) where
    pure t = pureMealy $ \s -> Emit t (pure t)

  instance profunctorMealy :: (Monad f) => Profunctor (MealyT f) where
    dimap l r = remap where
      remap m = mealy $ \s -> g <$> stepMealy (l s) m where
                g (Emit c m') = Emit (r c) (remap m')
                g Halt        = Halt

  instance semigroupMealy :: (Monad f) => Semigroup (MealyT f s a) where
    (<>) l r = mealy $ \s ->  let g (Emit c l') = pure $ Emit c (l' <> r)
                                  g Halt        = stepMealy s r
                              in  stepMealy s l >>= g

  instance monoidMealy :: (Monad f) => Monoid (MealyT f s a) where
    mempty = mealy $ \s -> pure Halt

  instance semigroupoidMealy :: (Monad f) => Semigroupoid (MealyT f) where
    (<<<) f g =
      mealy $ \b -> stepMealy b g >>= gb 

      where gb Halt = pure Halt
            gb (Emit c g') = fc <$> stepMealy c f where
              fc (Emit d f') = Emit d (f' <<< g')

  instance categoryMealy :: (Monad f) => Category (MealyT f) where
    id = pureMealy $ \t -> Emit t id

  instance arrowMealy :: (Monad f) => Arrow (MealyT f) where
    arr f = loop where loop = pureMealy $ \b -> Emit (f b) loop

    first m = mealy $ \s -> let b = fst s
                                d = snd s
                                g (Emit c f') = Emit (Tuple c d) (first f')
                                g Halt        = Halt
                            in  g <$> stepMealy b m 
    
  instance bindMealy :: (Monad f) => Bind (MealyT f s) where
    (>>=) m f = mealy $ \s -> let g (Emit a m') = h <$> stepMealy s (f a) where
                                                  h (Emit b bs) = Emit b (bs <> (m' >>= f))
                                                  h Halt        = Halt
                                  g Halt        = pure Halt                                    
                              in  stepMealy s m >>= g

  instance monadMealy :: (Monad f) => Monad (MealyT f s)
                                  
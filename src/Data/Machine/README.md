# Module Documentation

## Module Data.Machine.Mealy

#### `MealyT`

``` purescript
data MealyT f s a
  = MealyT (f (s -> f (Step f s a)))
```


#### `Step`

``` purescript
data Step f s a
  = Emit a (MealyT f s a)
  | Halt 
```


#### `Source`

``` purescript
type Source f s = MealyT f Unit s
```


#### `Sink`

``` purescript
type Sink f a = MealyT f a Unit
```


#### `source`

``` purescript
source :: forall f s. (Monad f) => f s -> Source f s
```


#### `sink`

``` purescript
sink :: forall f a. (Monad f) => (a -> f Unit) -> Sink f a
```


#### `runMealy`

``` purescript
runMealy :: forall f. (Monad f) => MealyT f Unit Unit -> f Unit
```


#### `stepMealy`

``` purescript
stepMealy :: forall f s a. (Monad f) => s -> MealyT f s a -> f (Step f s a)
```


#### `pureMealy`

``` purescript
pureMealy :: forall f s a. (Applicative f) => (s -> Step f s a) -> MealyT f s a
```


#### `mealy`

``` purescript
mealy :: forall f s a. (Applicative f) => (s -> f (Step f s a)) -> MealyT f s a
```


#### `halt`

``` purescript
halt :: forall f s a. (Applicative f) => MealyT f s a
```


#### `take`

``` purescript
take :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a
```


#### `drop`

``` purescript
drop :: forall f s a. (Monad f) => Number -> MealyT f s a -> MealyT f s a
```


#### `loop`

``` purescript
loop :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a
```


#### `zipWith`

``` purescript
zipWith :: forall f s a b c. (Monad f) => (a -> b -> c) -> MealyT f s a -> MealyT f s b -> MealyT f s c
```


#### `scanl`

``` purescript
scanl :: forall f s a b. (Monad f) => (b -> a -> b) -> b -> MealyT f s a -> MealyT f s b
```


#### `collect`

``` purescript
collect :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s [a]
```


#### `singleton`

``` purescript
singleton :: forall f s a. (Monad f) => a -> MealyT f s a
```


#### `fromMaybe`

``` purescript
fromMaybe :: forall f s a. (Monad f) => M.Maybe a -> MealyT f s a
```


#### `fromArray`

``` purescript
fromArray :: forall f s a. (Monad f) => [a] -> MealyT f s a
```


#### `wrapEffect`

``` purescript
wrapEffect :: forall f s a. (Monad f) => f a -> MealyT f s a
```


#### `msplit`

``` purescript
msplit :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s (M.Maybe (Tuple a (MealyT f s a)))
```

#### `interleave`

``` purescript
interleave :: forall f s a. (Monad f) => MealyT f s a -> MealyT f s a -> MealyT f s a
```


#### `ifte`

``` purescript
ifte :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b -> MealyT f s b
```


#### `(>>-)`

``` purescript
(>>-) :: forall f s a b. (Monad f) => MealyT f s a -> (a -> MealyT f s b) -> MealyT f s b
```


#### `functorMealy`

``` purescript
instance functorMealy :: (Monad f) => Functor (MealyT f s)
```


#### `applyMealy`

``` purescript
instance applyMealy :: (Monad f) => Apply (MealyT f s)
```


#### `applicativeMealy`

``` purescript
instance applicativeMealy :: (Monad f) => Applicative (MealyT f s)
```


#### `profunctorMealy`

``` purescript
instance profunctorMealy :: (Monad f) => Profunctor (MealyT f)
```


#### `strongMealy`

``` purescript
instance strongMealy :: (Monad f) => Strong (MealyT f)
```


#### `semigroupMealy`

``` purescript
instance semigroupMealy :: (Monad f) => Semigroup (MealyT f s a)
```


#### `monoidMealy`

``` purescript
instance monoidMealy :: (Monad f) => Monoid (MealyT f s a)
```


#### `semigroupoidMealy`

``` purescript
instance semigroupoidMealy :: (Monad f) => Semigroupoid (MealyT f)
```


#### `categoryMealy`

``` purescript
instance categoryMealy :: (Monad f) => Category (MealyT f)
```


#### `arrowMealy`

``` purescript
instance arrowMealy :: (Monad f) => Arrow (MealyT f)
```


#### `bindMealy`

``` purescript
instance bindMealy :: (Monad f) => Bind (MealyT f s)
```


#### `monadMealy`

``` purescript
instance monadMealy :: (Monad f) => Monad (MealyT f s)
```


#### `altMealy`

``` purescript
instance altMealy :: (Monad f) => Alt (MealyT f s)
```


#### `plusMealy`

``` purescript
instance plusMealy :: (Monad f) => Plus (MealyT f s)
```


#### `alternativeMealy`

``` purescript
instance alternativeMealy :: (Monad f) => Alternative (MealyT f s)
```


#### `monadPlus`

``` purescript
instance monadPlus :: (Monad f) => MonadPlus (MealyT f s)
```





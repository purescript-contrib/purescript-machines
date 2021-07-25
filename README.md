# Machines

[![CI](https://github.com/purescript-contrib/purescript-machines/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-machines/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-machines.svg)](https://github.com/purescript-contrib/purescript-machines/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-machines/badge)](http://pursuit.purescript.org/packages/purescript-machines)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](http://github.com/thomashoneyman)
[![Maintainer: paluh](https://img.shields.io/badge/maintainer-paluh-teal.svg)](http://github.com/paluh)

Machines is a library for building finite state machines. Finite state machines are useful for modeling many concerns that developers face. They help you describe a set of possible states and rules for how to transition from one state to another. The result is a complete view of possible states and transitions between states.

Currently this library implements [Mealy machines](https://en.wikipedia.org/wiki/Mealy_machine) with halting.

## Installation

Install `machines` with [Spago](https://github.com/purescript/spago):

```sh
spago install machines
```

## Quick start

Mealy machines are finite state machines. The `MealyT f i o` type represents a machine where `f` is the effect used for evaluation, `i` is the input, and `o` is the output value. The examples here use `Identity` as the effect type for simplicity, but you would usually use a different `Monad` such as `Effect`, `Aff`, or `State`.

There are several ways to build machines. One way is to use `do` syntax,
for example:

```purescript
import Prelude

import Control.MonadZero (guard)
import Data.Machine.Mealy (MealyT, fromArray, toUnfoldable)
import Data.Identity (Identity)

machine1 :: MealyT Identity Unit String
machine1 = do
  number <- fromArray [10, 20, 30, 40, 50, 0, 60, 70]
  guard (number /= 0)
  let scaled = div number 2
  pure $ show scaled
```

This will create a machine `machine1` which goes through the "inputs"
from the array. It then checks and halts on any zero input, and otherwise
scales the inputs (by dividing by 2). The result is then transformed into a string.

The resulting machine can be materialized via

```purescript
> toUnfoldable unit machine1 :: Array String
["5","10","15","20","25"]
```

Another way to write the same machine is using machine composition. In this example, we will be creating multiple machines using `pureMealy`, which relies on `Step`s.

A `Step f i o` represents a state transition in the machine. When you run a machine you are executing a series of steps. At each step the machine can stop via the `Halt` constructor or `Emit` a value and construct the rest of the machine.

```purescript
import Prelude

import Data.Identity (Identity)
import Data.Machine.Mealy (MealyT, Step(..), fromArray, pureMealy)

machine2 :: MealyT Identity Unit String
machine2 =
    fromArray [10, 20, 30, 40, 50, 0, 60, 70]
        >>> pureMealy haltOn0
        >>> pureMealy scale
        >>> pureMealy pretty
  where
    haltOn0 :: Int -> Step Identity Int Int
    haltOn0 0 = Halt
    haltOn0 n = Emit n $ pureMealy haltOn0

    scale :: Int -> Step Identity Int Int
    scale n = Emit (n `div` 2) $ pureMealy scale

    pretty :: Int -> Step Identity Int String
    pretty n = Emit (show n) $ pureMealy pretty
```

This machine does the same thing, except it creates multiple machines:

- `fromArray [10, 20 ...` is a `MealyT Identity Unit Int` which generates
    the integerers in the provided array,
- `pureMealy haltOn0` is a `MealyT Int Int` which halts on 0,
- `pureMealy scale` is a `MealyT Int Int` which scales the inputs, and
- `pureMealy pretty` is a `MealyT Int String` which converts inputs
    from integers to strings.

## Documentation

`machines` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-machines).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-machines/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://discord.com/invite/sMqwYUbvz6) chat.

## Contributing

You can contribute to `machines` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-machines/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

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

The quick start hasn't been written yet. Contributions are welcome!

## Documentation

`machines` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-machines).
2. Written documentation and [the changelog](./docs/CHANGELOG.md) are kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test)

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-machines/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to `machines` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-machines/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./.github/CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

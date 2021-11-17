# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#51 by @thomashoneyman)

## [v6.1.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v6.1.0) - 2021-05-06

New features:
- Added `once` to the export list of `Data.Machine.Mealy` (#49 by @PureFunctor)

Other improvements:
- Removed unused names and exported unused declaration found by the v0.14.1 PureScript release (#49 by @PureFunctor)

## [v6.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#46)
- Removed the `MonadZero` instance, as the type class has been deprecated in PureScript 0.14. Use `Monad` + `Alternative` constraints instead. (#46)

Other improvements:
- Added @paluh as a maintainer and removed @garyb (#27)
- Added a quick start to the README (#38 by @vladciobanu)
- Added module documentation to `Data.Machine.Mealy` (#39 by @vladciobanu)
- Loosened constraints on functions and instances (#43 by @mhmdanas)
- Renamed type variables of `MealyT` (#44 by @paluh)
- Renamed type variables of `Source` and `Sink` to match type arguments of `MealyT` (#26)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#31, #37)

## [v5.1.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v5.1.0) - 2018-06-06

- Added `hoistMealyT` (@CarstenKoenig)

## [v5.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v5.0.0) - 2018-05-25

- Updated for PureScript 0.12

## [v4.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v4.0.0) - 2017-04-03

- Updated for PureScript 0.11

## [v3.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v3.0.0) - 2016-11-25

- Removed unused `f` from inner function

## [v2.0.1](https://github.com/purescript-contrib/purescript-machines/releases/tag/v2.0.1) - 2016-11-22

- Fixed shadowed name warnings

## [v2.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v2.0.0) - 2016-10-17

- Updated dependencies

## [v1.0.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v1.0.0) - 2016-06-02

- Updated for 1.0 core libraries

## [v0.8.1](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.8.1) - 2016-01-06

- Added `Lazy` instance

## [v0.8.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.8.0) - 2015-10-14

- Added Travis build; fixed name shadowing (#10)

## [v0.7.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.7.0) - 2015-07-08

- Updated for PureScript 0.7 (#9)

## [v0.6.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.6.0) - 2015-04-01

- Updated dependencies (#4)

## [v0.5.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.5.0) - 2015-02-21

- Updated dependencies. **This release requires PureScript v0.6.8 or later.**

## [v0.4.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.4.0) - 2015-01-29

- Updated for changes in `purescript-arrows`

## [v0.2.0](https://github.com/purescript-contrib/purescript-machines/releases/tag/v0.2.0) - 2014-12-30

- Initial versioned release. Bumped `arrays` dependency.

# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.1.0](https://github.com/purescript/purescript-tailrec/releases/tag/v6.1.0) - 2022-05-16

New features:
- Added `tailRec2`, `tailRec3`, `loop2`, `loop3` convenience functions (#40 by @rhendric)

## [v6.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v6.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#38 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:

## [v5.0.1](https://github.com/purescript/purescript-tailrec/releases/tag/v5.0.1) - 2021-04-27

Other improvements:
- Fix warnings revealed by v0.14.1 PS release (#37 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#32)

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#33)
- Added a changelog and pull request template (#34, #35)

## [v4.1.1](https://github.com/purescript/purescript-tailrec/releases/tag/v4.1.1) - 2020-03-12

- Fixed outdated type signatures in readme and doc-comments (@thautwarm, #29)
- Fixed CI (@hdgarrood, #30)

## [v4.1.0](https://github.com/purescript/purescript-tailrec/releases/tag/v4.1.0) - 2019-08-27

- Added `whileJust` and `untilJust` (@safareli)

## [v4.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v4.0.0) - 2018-05-23

- Updated for PureScript 0.12

## [v3.3.0](https://github.com/purescript/purescript-tailrec/releases/tag/v3.3.0) - 2017-06-04

- Added `MonadRec` for `Maybe` (@safareli)

## [v3.2.0](https://github.com/purescript/purescript-tailrec/releases/tag/v3.2.0) - 2017-05-27

- Implemented `MonadRec` for `Function` (@safareli)

## [v3.1.0](https://github.com/purescript/purescript-tailrec/releases/tag/v3.1.0) - 2017-04-20

- Removed a space leak in `tailRecEff` (@matthewleon)

## [v3.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v3.0.0) - 2017-03-26

- Updated for PureScript 0.11

## [v2.0.2](https://github.com/purescript/purescript-tailrec/releases/tag/v2.0.2) - 2017-02-14

- Avoid `Discard` constraints in upcoming 0.11 release

## [v2.0.1](https://github.com/purescript/purescript-tailrec/releases/tag/v2.0.1) - 2016-11-14

- Fixed shadowed name warning

## [v2.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v2.0.0) - 2016-10-05

- Updated dependencies (@nwolverson)
- A `Step` type is now used rather than `Either` to manage the looping behaviour (originally by @jacereda)

## [v1.0.0](https://github.com/purescript/purescript-tailrec/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

- Added `MonadRec` instance for `Either` (@jdegoes)

## [v0.3.1](https://github.com/purescript/purescript-tailrec/releases/tag/v0.3.1) - 2015-08-13

- Fixed warnings

## [v0.3.0](https://github.com/purescript/purescript-tailrec/releases/tag/v0.3.0) - 2015-06-30

- Initial 0.7 compiler release. This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.2.2](https://github.com/purescript/purescript-tailrec/releases/tag/v0.2.2) - 2015-03-20

- Updated docs

## [v0.2.0](https://github.com/purescript/purescript-tailrec/releases/tag/v0.2.0) - 2015-03-03

- Updated dependencies

## [v0.2.1](https://github.com/purescript/purescript-tailrec/releases/tag/v0.2.1) - 2015-03-03

- Removed FFI dependency in `tailRecEff`.

## [v0.1.2](https://github.com/purescript/purescript-tailrec/releases/tag/v0.1.2) - 2015-02-26

- Added `forever`.

## [v0.1.1](https://github.com/purescript/purescript-tailrec/releases/tag/v0.1.1) - 2015-02-15

- Added `tailRecM2` and `tailRecM3`.

## [v0.1.0](https://github.com/purescript/purescript-tailrec/releases/tag/v0.1.0) - 2015-01-19

- Initial release

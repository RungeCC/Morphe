# Morphe

A package for calling external [`Typst`](https://typst.app/) from Wolfram Language.

The name `Morphe` is from Greek, which means "form" or "type".

## Prerequirements

- [`Typst`](https://typst.app/) with version `>= 0.14.0`

  > actually `0.14.0` only, below this version is not supported, since we use `typst info` to validate typst version, for older versions, it does not exist; due to the same reason, version check is actually not implemented yet.
- Wolfram Language with version `>= 14.1.0` (though it may work with older versions, but not tested).
- Paclet [`ErrorTools`](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/ErrorTools/tutorial/ErrorHandlingWithErrorTools.html).

## Installation

TODO.

## Usage

See `Typis.wl`.

## TODOs

+ check arguments
+ frontend completion supports
+ direct output Wolfram expression into typst
+ encoding typst types, elements in Wolfram language
+ validate typst flags
+ [x] cache outputs
+ [x] validate typst
+ [x] multi-pages output support
+ better error handling and cache for multi-pages mode
+ debug mode with timer support
+ better README and a Wolfram documentation.
+ make a Paclet or ResourceFunction
+ move `Error.wl` into a seperated package

## License

MIT License.

# Scheme interpreter

This is a Scheme interpreter written in Haskell. It is an extended version
of the interpreter developed in 
[Write Yourself a Scheme](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## Installation

To install, run

```
cabal sandbox init
cabal install -j
```

## Usage

After installation, run

```
.cbal-sandbox/bin/scheme-interpreter-hs
```

to open the interactive scheme interpreter. If you would like to load a Scheme
library, type

```
.cbal-sandbox/bin/scheme-interpreter-hs <path-to-lib>
```

## Bugs

## Future releases

The following are some features I intend on implementing in the future:

   * Add a basic standard library
   * Add support for Vectors
   * Add support for floating point arithmetic
   * Add support for complex arithmetic


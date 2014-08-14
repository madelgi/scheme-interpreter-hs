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
.cbal-sandbox/bin/pdx-lisp
```

to open the interactive scheme interpreter. After running the interpreter,
you should be met with a `Lisp>>>` prompt. If you would like to load a scheme
file, use the `load` command. For example, to load the included standard library,
enter

```
Lisp>>> (load "libraries/base/stdlib.scm")
```

After doing so, you will be able to use any of the functions included in stdlib.scm.
To exit the interpreter, type `quit`.

## Bugs

## Future releases

The following are some features I intend on implementing in the future:

   * ~~add a basic standard library~~
   * Add support for Vectors (In progress)
   * Add support for floating point arithmetic
   * Add support for complex arithmetic


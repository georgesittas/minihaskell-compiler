## MiniHaskell Compiler

This is a comprehensive implementation of a compiler and interpreter for MiniHaskell, a small subset of Haskell. The MiniHaskell code is compiled into an intermediate representation called _intensional code_, which is then used as the source language for the execution process.

The goal for this project was to design, implement and grade the final assignment for the [Programming Language Principles](https://cgi.di.uoa.gr/~prondo/LANGUAGES/languages.html) course, which is taught by prof. [Panagiotis Rontogiannis](https://cgi.di.uoa.gr/~prondo/) in [DiT](https://www.di.uoa.gr/) (UoA). The [assignment description](https://github.com/GeorgeSittas/minihaskell-compiler/blob/main/project22-23.pdf) is also included (in Greek).

### Intensional Code

Intensional code is a dataflow programming language inspired by [Lucid](https://en.wikipedia.org/wiki/Lucid_(programming_language)), and has been described extensively in the [paper](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/CDA5800533BC35832DDC9587E15EFCE0/S0956796897002633a.pdf/firstorder_functional_languages_and_intensional_logic.pdf) "First-order functional languages and intensional logic", by P. Rontogiannis, W. W. Wadge.

### Usage

```bash
# Install Haskell via GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install dependencies
cabal install --lib parsec
cabal install --lib pretty-simple

# Expose packages so that linking works as expected
ghc-pkg expose parsec
ghc-pkg expose pretty
ghc-pkg expose mtl

# Compile the project
make

# Run the test suite
make tests

# Remove all generated files
make clean
```

### Note

As described in the related [articles](https://github.com/GeorgeSittas/minihaskell-compiler/tree/main/pdfs), this implementation could be made a lot more efficient. However, this was outside the project's scope, and hence the interpreter is expected to be rather slow. A faster implementation in Rust can be found [here](https://github.com/nikos-alexandris/ic).

### Issues

The [test suite](https://github.com/GeorgeSittas/minihaskell-compiler/tree/main/tests) is certainly not exhaustive, so please consider [creating an issue](https://github.com/GeorgeSittas/minihaskell-compiler/issues/new) if you find a bug.

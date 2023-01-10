## MiniHaskell Compiler

This is a comprehensive implementation of a compiler and interpreter for MiniHaskell, a small subset of Haskell. The MiniHaskell code is compiled into an intermediate representation called _intensional code_, which is then used as the source language for the execution process.

The goal for this project was to design and implement the final assignment for the [Programming Language Principles](https://cgi.di.uoa.gr/~prondo/LANGUAGES/languages.html) course, which is taught by prof. [Panagiotis Rontogiannis](https://cgi.di.uoa.gr/~prondo/) in [DiT](https://www.di.uoa.gr/) (UoA). A [description](project22-23.pdf) of the assignment has also been provided (in Greek).

### Intensional Code

Intensional code is a dataflow programming language inspired by [Lucid](https://en.wikipedia.org/wiki/Lucid_(programming_language)), and has been described extensively in the [paper](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/CDA5800533BC35832DDC9587E15EFCE0/S0956796897002633a.pdf/firstorder_functional_languages_and_intensional_logic.pdf) "First-order functional languages and intensional logic".

### Usage

```bash
make
python test_script.py
make clean
```

### Note

As described in the related [articles](pdfs), this implementation could be made a lot more efficient. However, this was outside the project's scope, and hence the interpreter is expected to be rather slow.

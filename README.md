This is the sample code to accompany the book *Parallel and Concurrent Programming in Haskell* (Simon Marlow, O'Reilly 2013).

To build the code on your system, you need either:

* [Stack](http://haskellstack.org)
* [A Minimal GHC installation](https://www.haskell.org/downloads)
* [The Haskell Platform](https://www.haskell.org/downloads#platform)

## Building with Stack

```
stack build

# or using ghc-8.6.5
stack build --stack-yaml=stack-865.yaml
```

for windows

```
copy windowman.hs miscmodules/WindowManager.hs
copy tmvar.hs miscmodules/TMVar.hs
stack build
```

will build all the executables and install them in a platform-specific
subdirectory under `.stack-work/install`.

### Running with Stack

ch02

```
stack exec -- rpar 1 +RTS -N2
stack exec -- rpar 2 +RTS -N2
stack exec -- rpar 3 +RTS -N2

stack exec -- sudoku1 sudoku17.1000.txt
stack exec -- sudoku1 sudoku17.1000.txt +RTS -s
stack exec -- sudoku2 sudoku17.1000.txt +RTS -N2 -s
stack exec -- sudoku2 sudoku17.1000.txt +RTS -N2 -l
stack exec -- sudoku3 sudoku17.1000.txt +RTS -N2 -s
stack exec -- sudoku3 sudoku17.1000.txt +RTS -N2 -l
stack exec -- sudoku4 sudoku17.1000.txt +RTS -N2 -s
stack exec -- sudoku4 sudoku17.1000.txt +RTS -N2 -l
```

## Building with Cabal

```
cabal update
cabal build
```

### Running with Cabal

ch02

```
cabal exec -- rpar 1 +RTS -N2
cabal exec -- rpar 2 +RTS -N2
cabal exec -- rpar 3 +RTS -N2

cabal exec -- sudoku1 sudoku17.1000.txt
cabal exec -- sudoku1 sudoku17.1000.txt +RTS -s
cabal exec -- sudoku2 sudoku17.1000.txt +RTS -N2 -s
cabal exec -- sudoku2 sudoku17.1000.txt +RTS -N2 -l
cabal exec -- sudoku3 sudoku17.1000.txt +RTS -N2 -s
cabal exec -- sudoku3 sudoku17.1000.txt +RTS -N2 -l
cabal exec -- sudoku4 sudoku17.1000.txt +RTS -N2 -s
cabal exec -- sudoku4 sudoku17.1000.txt +RTS -N2 -l
```

## Building with Cabal v1-build

```
cabal sandbox init
cabal v1-install --only-dependencies
cabal v1-configure
cabal v1-build
```
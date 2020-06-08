This is the sample code to accompany the book *Parallel and Concurrent Programming in Haskell* (Simon Marlow, O'Reilly 2013).

To build the code on your system, you need either:

* [Stack](http://haskellstack.org)
* [A Minimal GHC installation](https://www.haskell.org/downloads)
* [The Haskell Platform](https://www.haskell.org/downloads#platform)

optional

```
sudo apt install gnuplot eog llvm-7 libdevil-dev
export PATH=/usr/lib/llvm-7/bin:$PATH
```

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

ch03

```
stack exec -- strat +RTS -N1 -s
stack exec -- strat +RTS -N2 -s

stack exec -- sudoku5 sudoku17.1000.txt +RTS -N2 -s
stack exec -- sudoku5 sudoku17.1000.txt +RTS -N2 -l

stack exec -- GenSamples 5 50000 100000 1010
stack exec -- kmeans seq
gnuplot -e 'set terminal png; plot "points"' > points.png
stack exec -- kmeans strat 64 +RTS -N2

echo 'Hello World!' | stack exec -- rsa encrypt -
echo 'Hello World!' | stack exec -- rsa encrypt - | stack exec -- rsa decrypt -

stack exec -- rsa encrypt /usr/share/dict/words >/dev/null +RTS -s
stack exec -- rsa1 encrypt /usr/share/dict/words >/dev/null +RTS -N4 -s
stack exec -- rsa2 encrypt /usr/share/dict/words >/dev/null +RTS -N4 -s -l

```

ch04

```
stack exec -- fwsparse 1000 800 +RTS -s
stack exec -- fwsparse1 1000 800 +RTS -s -N1
stack exec -- fwsparse1 1000 800 +RTS -s -N4

stack exec -- rsa-pipeline /usr/share/dict/words >/dev/null +RTS -s -l -N2

stack exec -- timetable 4 3 11 10 3 +RTS -s
stack exec -- timetable1 4 3 11 10 3 +RTS -s
stack exec -- timetable2 4 3 11 10 3 +RTS -s -N4
stack exec -- timetable3 4 3 11 10 3 +RTS -s -N4

stack exec -- parinfer <parinfer/benchmark.in +RTS -s
stack exec -- parinfer <parinfer/benchmark.in +RTS -s -N2
stack exec -- parinfer <parinfer/benchmark.in +RTS -s -N3
stack exec -- parinfer <parinfer/benchmark.in +RTS -s -N4
```

ch05

```
stack clean
stack build --flag parconc-examples:llvm
stack exec -- fwdense 500 +RTS -s

stack exec -- fwdense1 500 +RTS -s
stack exec -- fwdense1 500 +RTS -s -N4

stack clean
stack build --flag parconc-examples:devil --flag parconc-examples:llvm
stack exec -- rotateimage 4 cat.jpg straight.jpg
rm straight.jpg
stack exec -- rotateimage 4 cat.jpg straight.jpg +RTS -s
rm straight.jpg
stack exec -- rotateimage 4 cat.jpg straight.jpg +RTS -s -N4
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

ch03

```
cabal exec -- strat +RTS -N1 -s
cabal exec -- strat +RTS -N2 -s

cabal exec -- sudoku5 sudoku17.1000.txt +RTS -N2 -s
cabal exec -- sudoku5 sudoku17.1000.txt +RTS -N2 -l

cabal exec -- GenSamples 5 50000 100000 1010
cabal exec -- kmeans seq
gnuplot -e 'set terminal png; plot "points"' > points.png
cabal exec -- kmeans strat 64 +RTS -N2

echo 'Hello World!' | cabal exec -- rsa encrypt -
echo 'Hello World!' | cabal exec -- rsa encrypt - | cabal exec -- rsa decrypt -

cabal exec -- rsa encrypt /usr/share/dict/words >/dev/null +RTS -s
cabal exec -- rsa1 encrypt /usr/share/dict/words >/dev/null +RTS -N4 -s
cabal exec -- rsa2 encrypt /usr/share/dict/words >/dev/null +RTS -N4 -s -l
```

ch04

```
cabal exec -- fwsparse 1000 800 +RTS -s
cabal exec -- fwsparse1 1000 800 +RTS -s -N1
cabal exec -- fwsparse1 1000 800 +RTS -s -N4

cabal exec -- rsa-pipeline /usr/share/dict/words >/dev/null +RTS -s -l -N2

cabal exec -- timetable 4 3 11 10 3 +RTS -s
cabal exec -- timetable1 4 3 11 10 3 +RTS -s
cabal exec -- timetable2 4 3 11 10 3 +RTS -s -N4
cabal exec -- timetable3 4 3 11 10 3 +RTS -s -N4

cabal exec -- parinfer <parinfer/benchmark.in +RTS -s
cabal exec -- parinfer <parinfer/benchmark.in +RTS -s -N2
cabal exec -- parinfer <parinfer/benchmark.in +RTS -s -N3
cabal exec -- parinfer <parinfer/benchmark.in +RTS -s -N4
```

ch05

```
cabal clean
cabal build -fllvm
cabal exec -- fwdense 500 +RTS -s

cabal exec -- fwdense1 500 +RTS -s
cabal exec -- fwdense1 500 +RTS -s -N4

cabal clean
cabal build -fdevil -fllvm
cabal exec -fdevil -- rotateimage 4 cat.jpg straight.jpg
rm straight.jpg
cabal exec -fdevil -- rotateimage 4 cat.jpg straight.jpg +RTS -s
rm straight.jpg
cabal exec -fdevil -- rotateimage 4 cat.jpg straight.jpg +RTS -s -N4
```

## Building with Cabal v1-build

```
cabal sandbox init
cabal v1-install --only-dependencies
cabal v1-configure
cabal v1-build
```
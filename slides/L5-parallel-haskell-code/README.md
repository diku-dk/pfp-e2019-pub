# Example programs from the lecture

Run

```
cabal install --installdir=. --overwrite-policy=always
```

to build all the examples and put the result in the current directory
(well, symlinks, but close enough).

## Running single programs (recommended)

Example:

```
cabal run sudoku3 -- sudoku17.1000.txt +RTS -N4 -s
```

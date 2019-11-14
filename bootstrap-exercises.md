# Bootstrap Futhark exercises for the lab on 20/11

Due to unfortunate scheduling, you will likely not be able to make
much headway on the first weekly assignment before the lecture, which
is after the lab.  Hence you should spend the lab ensuring that you
are able to compile, run, and benchmark Futhark programs, either on
your own machine or on [DIKUs GPU
cluster](https://github.com/diku-dk/pfp-e2019-pub#practical-information).

The following exercises serve as a simple introduction to Futhark
programming.  They should be solvable using material from the Monday
lecture and [the Futhark book](https://futhark-book.readthedocs.io).

## Reimplement library functions

Implement the following Futhark utility functions:

```
let rotate [n] 't (r: i32) (xs: [n]t) : [n]t =
  ...

let transpose [n] [m] 't (xss: [n][m]t) : [m][n]t =
  ...

let concat [n] [m] 't (xs: [n]t) (ys: [m]t) : []t =
  ...

```

This is doable with `map`s (possibly nested) and explicit array
indexing.  [Make sure to also write
tests](https://futhark-book.readthedocs.io/en/latest/practical-matters.html#testing-and-debugging).
How fast do they run using the `opencl` backend compared to the `c`
backend?

## Implement 1D smoothing

A one-dimensional array `xs` can be *smoothed* by, for every index
`xs[i]`, recomputing it as `(x[i-1] + x[i] + x[i+1])/3`.  This
pattern, where we update a cell in a grid based on its neighbours, is
called a *stencil*.  Implement this one-dimensional smoothing in
Futhark.

What should you do for elements at the edge?

## Implement Game of Life

The [Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) is a
two-dimensional stencil.  Implement it in Futhark.

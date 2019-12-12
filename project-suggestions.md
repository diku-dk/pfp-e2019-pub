# Project suggestions

If you have your own idea for a project (perhaps based on other
coursework), feel free to suggest it.  Those tend to be the most
interesting.  Otherwise, we have the following list of suggested
projects.

## Clustered *n*-body simulation in Futhark

An *n*-body simulation is a program that computes the interaction over
time between *n* points moving around in some (typically
three-dimensional) space.  At each time step, each point performs a
*pair interaction* with all other points in the space.  The form of
the interaction can vary, but a simple example is gravitational
attraction.

In its naive formulation, an *n*-body time step is an *O(n²)*
algorithm, as each of the *n* points must look at all of the other
points.  We [have an implementation in
Futhark](https://github.com/diku-dk/futhark-benchmarks/tree/master/accelerate/nbody),
where the compiler is able to exploit the regular iteration nature of
the program to perform a very efficient form of one-dimensional
tiling, but as *n* increases, the asymptotics take over.

In large-scale *n*-body simulations, one typically uses tree-based
clustering techniques that aggregate together multiple points.  When
computing interactions, only nearby particles are treated
individually, while distant particles are treated as a single large
particle centered at their clusters' centre of mass.  An example is
the [Barnes-Hut
algorithm](https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation).

This project is about writing a Futhark implementation of the
Barnes-Hut algorithm.  One interesting subgoal is how to retain the
ability use the low-overhead *O(n²)* algorithm when computing nearby
interactions.

## Porting a Parboil benchmark to a parallel language

[Parboil](http://impact.crhc.illinois.edu/parboil/parboil.aspx) is a
suite of benchmarks that is used when presenting new research into
compilers or parallel programming.  This project is about picking one
of the benchmarks and porting it to a parallel language covered in
class (I prefer Futhark, but if you want to use `ispc` or Parallel
Haskell, that's fine too).  For Futhark, [we already we have
implementations of `histo`, `mri-q`, `sgemm`, `stencil`, and
`tpacf`](https://github.com/diku-dk/futhark-benchmarks/tree/master/parboil),
so we are mostly interested in the remaining ones.

## Implementing the *k*-nearest neighbours algorithm in Futhark

[*k*-NN](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm)
is a classification and regression algorithm.  In the classification
case, we classify new objects (identified by their position in some
*d*-dimensional space) by the classifications of the *k* nearest
already classified objects.  This algorithm sees wide use in data
science.

The project is about implementing *k*-NN in Futhark in such a way that
it scales well for different values of *k*, *d*, and *n* (the input
size).  Runtime comparison with some prior implementation of *k*-NN
should also be done.

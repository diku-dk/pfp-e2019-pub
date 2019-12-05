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

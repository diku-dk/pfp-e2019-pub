# Parallel Functional Programming, Block 2 2019

PFP is structured around five weeks with lectures and lab sessions
on Monday and Wednesday, followed by a final project to be
presented orally at the exam.  Throughout the course, you will hand in
four weekly assignments.  These *weeklies* count for 40\% of the
grade, while the exam counts for 60\%.

The teachers are **Cosmin Oancea** and **Troels Henriksen**.

All lectures and lab sessions will be delivered in English.  The
assignments and projects will be posted in English, and while you can
chose to hand in solutions in either English or Danish, English is
preferred.

All course material is distributed via this GitHub page.  Assignment
handin is still on Absalon.  There is no mandated textbook for the
course - you will be assigned reading material from papers and such.

## Course schedule

This course schedule is tentative and will be updated as we go along.

The lab sessions are aimed at providing help for the weeklies and
group project.  Do not assume you can solve them without showing up to
the lab sessions.

On Monday, we are in "Kursussal 3" at "Zoo" (a building connected to
the museum of zoology).

On Wednesday, we are at DIKU in classroom 1-0-22 from 10:00-12:00 and
in classrom 1-0-26 from 13:00-15:00.

**Note that the order of labs and lectures are swapped for the first
three teaching days.**

| Date | Time | Topic | Material |
| --- | --- | --- | --- |
| 18/11 | 13:00-15:00 | *Cancelled* |
| 18/11 | 15:00-17:00 | [Intro, deterministic parallelism, data parallelism, Futhark](slides/L1-determ-prog.pdf) | [Parallel Programming in Futhark](https://futhark-book.readthedocs.io/en/latest/), sections 1-4 | |
| 20/11 | 10:00-12:00 | Lab ([**Assignment 1 handout**](weekly-1/)) | [Futhark exercises](bootstrap-exercises.md) |
| 20/11 | 13:00-15:00 | [Cost models, advanced Futhark](slides/L2-advanced-futhark-cost-models.pdf) | [Guy Blelloch: Programming Parallel Algorithms](material/blelloch-programming-parallel-algorithms.pdf), [Prefix Sums and Their Applications](material/prefix-sums-and-their-applications.pdf), [A Provable Time and Space Efficient Implementation of NESL](material/a-provable-time-and-space-efficient-implementation-of-nesl.pdf) |
| 25/11 | 13:00-15:00 | Lab | |
| 25/11 | 15:00-17:00 | [Regular flattening: moderate and incremental](slides/L3-regular-flattening.pdf) | [Futhark: Purely Functional GPU-Programming with Nested Parallelism and In-Place Array Updates](https://futhark-lang.org/publications/pldi17.pdf), [Incremental Flattening for Nested Data Parallelism](https://futhark-lang.org/publications/ppopp19.pdf) (particularly the latter) |
| 27/11 | 10:00-12:00 | [Full/irregular flattening](slides/L4-irreg-flattening.pdf) | [Transforming High-Level Data-Parallel Programs into Vector Operations](material/flattening/NeslFlatTechPaper.pdf), [Harnessing the Multicores: Nested Data Parallelism in Haskell](material/flattening/harnessing-multicores.pdf) (not easy to read)|
| 27/11 | 13:00-15:00 | Lab ([**Assignment 2 handout**](weekly-2/)) | |
| 2/12 | 13:00-15:00 | [Task parallelism (parallel Haskell)](slides/L5-parallel-haskell.pdf/) ([code](slides/L5-parallel-haskell-code/)) | [Parallel and Concurrent Programming in Haskell](https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/), chapter 4. [Seq no more: Better Strategies for Parallel Haskell](material/seq-no-more.pdf) |
| 2/12 | 15:00-17:00 | Lab | |
| 4/12 | 10:00-12:00 | [Halide](slides/L7-Halide.pdf) | [Halide: A Language and Compiler for Optimizing Parallelism, Locality and Recomputation in Image Processing Pipelines](material/halide-pldi13.pdf) | |
| 4/12 | 13:00-15:00 | Lab ([**Assignment 3 handout**](weekly-3)) | |
| 9/12 | 13:00-15:00 | [Polyhedral Analysis](slides/L9-polyhedral.pdf) | [PMPH Dependence Analysis](material/poly/L5-LoopParI.pdf); [Sven Verdoolaege: Presburger Formulas and Polyhedral Compilation (tutorial)](material/poly/polycomp-tutorial.pdf); [Sven Verdoolaege: Presburger Sets and Relations: from High-Level Modelling to Low-Level Implementation (slides)](material/poly/poly-in-detail.pdf)
| 9/12 | 15:00-17:00 | Lab | [Code Examples Using the ISLPY library](material/poly/poly-code-egs/) |
| 11/12 | 10:00-12:00 | [Vector programming with ISPC](slides/L8-ispc.pdf) | [ispc: A SPMD Compiler for High-Performance CPU Programming](material/ispc_inpar_2012.pdf) |
| 11/12 | 13:00-15:00 | Lab ([**Assignment 4 handout**](weekly-4)) | |
| 16/12 | 13:00-15:00 | [Optimizing Locality of Reference](slides/L10-LocOfRef.pdf) | [Related Literature](material/Opt-Loc-Ref-Lit/) |
| 16/12 | 15:00-17:00 | Lab (with project proposals) | |
| 18/12 | 10:00-12:00 | Lab | |
| 18/12 | 13:00-15:00 | Lab | |

## Weekly assignments

The weekly assignments are **mandatory**, must be solved
**individually**, and make up 40% of your final grade.  Submission is
on Absalon.

You will receive feedback a week after the handin deadline (at the
latest).  You then have another week to prepare a resubmission.  That
is, **the resubmission deadline is two weeks after the original handin
deadline**.

### Weekly 1 (due November 28)

* [Assignment text](weekly-1/1.pdf)
* [Code handout](weekly-1/ising-handout.tar.gz)
* [Hopefully helpful notes on how to do these kinds of assignments](weekly-1/1-notes.pdf)

### Weekly 2 (due December 5)

* [Assignment text](weekly-2/2.pdf)
* [Code handout](weekly-2/code.tar.gz)

### Weekly 3 (due December 12)

* [Assignment text](weekly-3/3.pdf)
* [Code handout](weekly-3/code-handout)

### Weekly 4 (due December 19)

* [Assignment text](weekly-4/4.pdf)
* [Code handout](weekly-4/code-handout)

## Group project

[Project suggestions here.](project-suggestions.md)

## Practical information

You may find it useful to make use of DIKUs GPU machines in your work.
You log in by first SSHing to the bastion server
`ssh-diku-apl.science.ku.dk` using your KU license plate (`abc123`) as
the user name, and then SSHing on to one of the GPU machines.  Despite
their names, they each have two multi-core CPUs and plenty of RAM as
well.

  * `gpu01-diku-apl`, `gpu02-diku-apl`, `gpu03-diku-apl` have dual GTX
    780 Ti GPUs.

  * `phi-diku-apl` has a K40 GPU.

  * `gpu04-diku-apl` has a GTX 2080 Ti GPU (by far the fastest).

All machines should have all the software installed you need.  If you
are missing something, [contact Troels](mailto:athas@sigkill.dk).  The
machines have a shared home directory (which is very slow), *except*
`gpu01-diku-apl`, which has its own home directory (which is a little
faster).

### GPU setup

For CUDA to work, you may need to add the following to your `$HOME/.bash_profile`:

```bash
CUDA_DIR=/usr/local/cuda
export PATH=$CUDA_DIR/bin:$PATH
export LD_LIBRARY_PATH=$CUDA_DIR/lib64:$LD_LIBRARY_PATH
export LIBRARY_PATH=$LD_LIBRARY_PATH:$LIBRARY_PATH
export CPLUS_INCLUDE_PATH=$CUDA_DIR/include:$CPLUS_INCLUDE_PATH
export C_INCLUDE_PATH=$CUDA_DIR/include:$C_INCLUDE_PATH
```

## Other resources

You are not expected to read/watch the following unless otherwise
noted, but they contain useful and interesting background information.

* [The Futhark User's Guide](https://futhark.readthedocs.io), in
  particular [Futhark Compared to Other Functional
  Languages](https://futhark.readthedocs.io/en/latest/versus-other-languages.html)

* [Troels' PhD thesis on the Futhark compiler](https://futhark-lang.org/publications/troels-henriksen-phd-thesis.pdf)

* [A library of parallel algorithms in NESL](http://www.cs.cmu.edu/~scandal/nesl/algorithms.html)

* [Functional Parallel Algorithms by Guy Blelloch](https://vimeo.com/showcase/1468571/video/16541324)

* ["Performance Matters" by Emery Berger](https://www.youtube.com/watch?v=r-TLSBdHe1A)

* [The story of `ispc`](https://pharr.org/matt/blog/2018/04/18/ispc-origins.html) (you can skip the stuff about office politics, although it might ultimately be the most valuable part of the story)

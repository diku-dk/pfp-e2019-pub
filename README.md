# Parallel Functional Programming, Block 2 2019

PFP is structured around five weeks of with lectures and lab sessions
on Monday and Wednesday, followed by a final project, which will be
presented orally at the exam.  Throughout the lecture weeks you will
hand in three assignments.  These assignments, together with the exam,
will form the basis for your grade.

The teachers are **Cosmin Oancea** and **Troels Henriksen**.

All lectures and lab sessions will be delivered in English.  The
assignments and projects will be posted in English, and while you can
chose to hand in solutions in either English or Danish, English is
preferred.

All course material is distributed via this GitHub page.  Assignment
handin is still on Absalon.  There is no mandated textbook for the
course - you will be assigned reading material from papers and such.

## Course schedule

This course schedule is tentative and will be updated.  **In
particular:** it is likely that we may swap the order of lectures and
labs for the first three teaching days, due to schedule conflicts.

The lab sessions are aimed at providing help for the weekly assignment
and group project.  Do not assume you can solve them without showing
up to the lab sessions.

On Monday, we are in "Kursussal 3" at "Zoo" (a building connected to
the museum of zoology).

On Wednesday, we are at DIKU in classroom 1-0-22 from 10:00-12:00 and
in classrom 1-0-26 from 13:00-15:00.

| Date | Time | Topic | Material |
| --- | --- | --- | --- |
| 18/11 | 13:00-15:00 | Intro, deterministic parallelism, data parallelism, Futhark | [Parallel Programming in Futhark](https://futhark-book.readthedocs.io/en/latest/), sections 1-4 |
| 18/11 | 15:00-17:00 | Lab | |
| 21/11 | 10:00-12:00 | Cost models, advanced Futhark | |
| 21/11 | 13:00-15:00 | Lab | |
| 25/11 | 13:00-15:00 | Regular flattening: moderate flattening, incremental flattening | |
| 25/11 | 15:00-17:00 | Lab | |
| 27/11 | 10:00-12:00 | Full/irregular flattening | |
| 27/11 | 13:00-15:00 | Lab | |
| 2/12 | 13:00-15:00 | Task parallelism (parallel Haskell) | |
| 2/12 | 15:00-17:00 | Lab | |
| 4/12 | 10:00-12:00 | Vector programming with ISPC | |
| 4/12 | 13:00-15:00 | Lab | |
| 9/12 | 13:00-15:00 | Halide | |
| 9/12 | 15:00-17:00 | Lab | |
| 11/12 | 10:00-12:00 | Polyhedral I | |
| 11/12 | 13:00-15:00 | Lab | |
| 16/12 | 13:00-15:00 | Polyhedral II | |
| 16/12 | 15:00-17:00 | Lab | |
| 18/12 | 10:00-12:00 | Irregular locality of reference | |
| 18/12 | 13:00-15:00 | Lab (with project proposals) | |

## Assignments

TBA.

## Practical information

You may find it useful to make use of DIKUs GPU machines in your work.
You log in by first SSHing to the bastion server
`ssh-diku-apl.science.ku.dk` using your KU license plate (`abc123`) as
the user name, and then SSHing on to one of the GPU machines:

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

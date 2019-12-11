###
#           for(i=1; i<=N; i++)
# S1[i]:       Y[i] = f(Z[i]);
# S2[i]:       X[i] = g(Y[i-1]);
#
#                 |
#                 v
# S2[0]:   if(N>=1) X[1] = Y[0]
#           for(p=1; p<=N-1; p++)
# S1[p]:       Y[p] = f(Z[p]);
# S2[p]:       X[p+1] = g(Y[p]);
# S1[N]:   if(N>=1) Y[N] = Z[N]

import islpy as isl
import common

### fill in the original iteration space (domain)---set of stmt instances
I = isl.UnionSet("[N] -> { ... }")

### fill in the original schedule---relation mapping stmts to time
Sini = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
print("Initial Schedule:")
print(Sini);

### fill in the write/read access relations---mapping stmts to read/written array elements
Write  = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
Read   = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
Dep = common.mkDepGraph(Sini, Read, Write)
print("Dependency graph is:")
print(Dep);

### Most Importantly: fill in the new schedule representing the 
###                   re-indexing transformation as a relation mapping
###                   original stmts to their new (rescheduled) time.
### Please write the whole scheduling, including the presburger formulas,
###    (rather than intersecting with the `I` domain).
### Then verify that the transformed loop of index `p` is parallel
###    (by writing a new schedule for it)!
###
### Hint: The most natural way to represent the time is by 3 dimensions:
###         - one at the outer level (the two `if` stmtms and the loop)
###         - one for the loop (middle)
###         - one that defines the order of stmts inside the loop (inner)
###       With the new schedule, a loop iteration `p` contains `S1` from
###         the same iteration `p` of the original loop and `S2` from the
###         next iteration `p+1`of the original loop. The first instance
###         of `S2` (`p=1` in the original loop) is executed before the 
###         transformed loop and the last instance of `S1` (`p=N`) is 
###         executed after the transformed loop.
###       Please note that the conditions of the `if` branches should
###         also be encoded as Presburger formulas (constraints).
Snew = isl.UnionMap("[N] -> { ... }")
print("Transformed Schedule:")
print(Snew);

(timesrcsink, is_empty) = common.checkTimeDepsPreserved(Snew, Dep)
print("Time Dependencies (Src -> Sink) On New Schedule:");
print(timesrcsink);
print("Dependencies Respected by New Schedule:")
print(is_empty);

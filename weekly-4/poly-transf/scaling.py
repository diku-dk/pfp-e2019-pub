###
#           for(i=1; i<=N; i++)
# S1[i]:       Y[2*i] = f(Z[2*i]);
#           for(j=1; j<=2*N; j++)
# S2[j]:       X[j] = g(Y[j]);
#
#                 |
#                 v
#
#           for(p=1; p<=2*N; p++)
#              if ((p mod 2) == 0)
# S1[p/2]:        Y[p] = f(Z[p]);
# S2[p]:       X[p] = g(Y[p]);
#
import islpy as isl
import common

### fill in the original iteration space (domain)---set of stmt instances
I = isl.UnionSet("[N] -> { ... }")

### fill in the original schedule---relation mapping stmts to time
###   Most naturally, the time can be represented in two dimensions:
###     - one that orders the two loops, i.e., all the statements of
###         the first loop come before any statement of the second loop;
###     - one that orders the iterations inside each loop;
Sini = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
print("Initial Schedule:")
print(Sini);

### fill in the write/read access relations---mapping stmts to 
###   read/written array elements
Write = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
Read  = isl.UnionMap("[N] -> { ... }").intersect_domain(I)

Dep = common.mkDepGraph(Sini, Read, Write)
print("Dependency graph is:")
print(Dep);

### Most Importantly: fill in the new schedule representing the 
###                   scaling transformation as a relation mapping
###                   original stmts to their new (rescheduled) time.
### Please write the whole scheduling, including the presburger formulas,
###    (rather than intersecting with the `I` domain).
###
### Hint: The most natural way to represent the time is still by 2 dimensions:
###         - one for the loop (outer)
###         - one for ordering the statement inside the loop.
###       The non-trivial part is to write the rescheduling of statement `S1`:
###         - the `if` condition needs to be encoded as a Presburger formula;
###         - in the original loop the `S1` has instances `S1[i]` where `i` goes 
###             from `1 ... N`;
###         - hence in the transform loop they will be `S1[q]` where `q=p/2`, and
###           `p` is even, and the range of `p` is from `1...2*N`.
###         - You'll need to re-write all that above as a Presburger formula;
###           `x mod k`, `x/k` and `and` are supported by the underlying library (islpy) 
###
Snew = isl.UnionMap("[N] -> { ... }")
print("Transformed Schedule:")
print(Snew);

(timesrcsink, is_empty) = common.checkTimeDepsPreserved(Snew, Dep)
print("Time Dependencies (Src -> Sink) On New Schedule:");
print(timesrcsink);
print("Dependencies Respected by New Schedule:")
print(is_empty);

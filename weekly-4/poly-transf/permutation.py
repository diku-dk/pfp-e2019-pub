###
#           for(i=1; i<=N; i++)
#             for(j=0; j<=N; j++)
# S1[i,j]:      Z[i,j] = f(Z[i-1,j]);
#
#                     |
#                     v
#
#           for(j=0; j<=N; j++)
#             for(i=1; i<=N; i++)
# S1[i,j]:      Z[i,j] = f(Z[i-1,j]);
#
import islpy as isl
import common

### fill in the original iteration space (domain)---set of stmt instances
I = isl.UnionSet("[N] -> { ... }")

### fill in the original schedule---relation mapping stmts to time
Sini  = isl.UnionMap("[N] -> { ... }").intersect_domain(I)
print("Initial Schedule:")
print(Sini);

### fill in the write/read access relations---mapping stmts to read/written array elements
Write  = isl.UnionMap("[N] -> { ... }").intersect_domain(I) 
Read   = isl.UnionMap("[N] -> { ... }").intersect_domain(I) 

Dep = common.mkDepGraph(Sini, Read, Write)
print("Dependency graph is:")
print(Dep);

### Most Importantly: fill in the new schedule representing the loop
###                   interchange transformation as a relation mapping
###                   original stmts to their new (rescheduled) time.
### Then verify that loop of index `j` is parallel (by writing a new
###    schedule for it)!
Snew = isl.UnionMap("[N] -> { ... }").intersect_domain(I) 
print("Transformed Schedule:")
print(Snew);

(timesrcsink, is_empty) = common.checkTimeDepsPreserved(Snew, Dep)
print("Time Dependencies (Src -> Sink) On New Schedule:");
print(timesrcsink);
print("Dependencies Respected by New Schedule:")
print(is_empty);

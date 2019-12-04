#include <stdlib.h>
#include <stdio.h>
#include <cstdint>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <time.h> 
#include <iostream>

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}


/***********************/
/*** SIZES AND TILES ***/
/***********************/
#ifndef DIM_X
#define DIM_X (64*1024*1024) //3072
#endif

#ifndef TILE
#define TILE  (2048)
#endif

#ifndef ITER
#define ITER  128
#endif

#ifndef FUSEDEG
#define FUSEDEG 64
#endif

#define PAD     256   // implicit assumption: PAD >= FUSEDEG!
/************************/

#define RUNS    5
#define EPS     0.000001

#define MIN(x,y)    ((x)<(y)? (x) : (y))
#define MAX(x,y)    ((x)>(y)? (x) : (y))

template<class NUM>
void validate(const int N, NUM* a1, NUM* a2) {
    for(int32_t i=0; i<N; i++) {
        NUM v1 = a1[i];
        NUM v2 = a2[i];
        if(fabs((float)(v1-v2)) > EPS) {
            std::cout<<"INVALID at index: "<<i<<", v1: "<<v1<<" v2: "<<v2<<"\n";
            exit(1);
        }
    }
    printf("VALIDATES!!!\n");
}

inline int32_t modX(const int32_t ind) { return ((ind+DIM_X) % DIM_X); }

template<class NUM>
NUM* breadthFirst(NUM* inp1, NUM* out1) {
    NUM *out20, *out, *inp = out1;
    { // allocate and initialized double buffer arrays:
        const uint32_t DIM_PAD = DIM_X+2*PAD;

        out20 = (NUM*)malloc(DIM_PAD*sizeof(NUM));
        out   = out20 + PAD;
        out[-1]    = inp1[-1];
        out[DIM_X] = inp1[DIM_X];
        
        #pragma omp parallel for default(shared) schedule(static)
        for(int i=-PAD; i<DIM_X+PAD; i++) {
            out1[i] = inp1[i];
        }
    }

    // perform the iterative stencil
    for(int q=0; q<ITER; q++) {
        #pragma omp parallel for default(shared) schedule(static)
        for(int32_t x=0; x<DIM_X; x++) {
            NUM el0, elm1, elp1;
            el0  = inp[x];
            elm1 = inp[x-1];
            elp1 = inp[x+1];
            out[x] = (elm1 + el0 + elp1) / 3;
        }
        { // double buffering
            NUM* tmp;  tmp = inp;
            inp = out; out = tmp;
        }
    }

    // copy the result in out1 if neccessary
    if( (ITER % 2) == 1 ) { 
        #pragma omp parallel for default(shared) schedule(static)
        for(int i=0; i<DIM_X; i++) {
            out1[i] = inp[i];
        }
    }
    free(out20);
}

/**
 * Weekly 3, Task3: 
 *   Consider the simple 1d iterative stencil:
 *     `for(int i=0; i<ITER; i++) {
 *        for(int x=0; x<DIM_X; x++)
 *          out(x) = (inp(x-1)+inp(x)+inp(x+1))/3;
 *        tmp = inp; inp = out; out = tmp; // switch inp with out
 *      }`
 *   see also function `breadthFirst` above.
 *
 *   THIS TASK REFERS TO (unrolling and) FUSING THE STENCIL
 *     `FUSEDEG` times
 * 
 *   Template parameter NUM is the array-element type, and
 *     T is one-dimensional tile size (e.g., 1024 or 2048).
 *   Arguments:
 *     `inp1`  is the input  2D array
 *     `out1`  is the result 2D array
 *
 *   The input to stencil is a 1d array of length `DIM_X`,
 *     but it has been padded in both directions.
 * 
 *   The stencil implementation uses a padding technique
 *     for boundary cells: you can safely assume that you can
 *     access element `-FUSEDEG` and `DIM_X+FUSEDEG` on the
 *     input stencil.
 * 
 *   Please note that the tile `T` is known to evenly divide `DIM_X`!
 *
 *   ToDo: the code structure and OMP parallelization is already
 *         provided to you. Your task is to fill in the missing
 *         parts of the code at `1. FILL IN THE CODE HERE:`
 *         and see related hints. 
 */
template<class NUM, int32_t T>
void tiledFused(NUM* inp1, NUM* out1) {

    NUM *out20, *out, *inp = out1;
    { // allocate and initialized double buffer arrays:
        const uint32_t DIM_PAD = DIM_X+2*PAD;

        out20 = (NUM*)malloc(DIM_PAD*sizeof(NUM));
        out   = out20 + PAD;
        out[-1]    = inp1[-1];
        out[DIM_X] = inp1[DIM_X];
        
        #pragma omp parallel for default(shared) schedule(static)
        for(int i=-PAD; i<DIM_X+PAD; i++) {
            out1[i] = inp1[i];
        }
    }

    // perform the fused iterative stencil
    for(int qq=0; qq<ITER; qq+=FUSEDEG) {
        #pragma omp parallel default(shared)
        {
            // semantically `tile` should be `[2][-FUSEDEG .. T+FUSEDEG-1]`,
            // but since C++ does not supports it, we allocated as below;
            //   index `0` with the declaration "[-FUSEDEG .. T+FUSEDEG-1]"
            //   would correspond to index `FUSEDEG` in the implementation below.
            NUM tile[2][T+2*FUSEDEG];
            #pragma omp for schedule(static)
            for(int32_t tx=0; tx<DIM_X; tx+=T) {
                // semantically copy slice [tx-FUSEDEG: tx+FUSEDEG]
                // to `tile` array, which has been allocated to have
                // enough space [T+2*FUSEDEG]
                for(int32_t x = -FUSEDEG; x < T+FUSEDEG; x++) {
                    tile[0][x+FUSEDEG] = inp[tx+x];
                }
                if (tx==0) { tile[1][FUSEDEG-1] = inp[-1]; }
                if (tx+T >= DIM_X) { tile[1][FUSEDEG+T] = inp[DIM_X]; }
                
                // each fusion needs to compute one more element to the left
                // and one more to the right than what is going to be written
                // in the next step, i.e., considering indexing in the global
                //  input/output array of size DIM_X:
                //   iter q0=0 computes slice   [tx-FUSEDEG   : tx+T+FUSEDEG]
                //   iter q0=1 computes slice   [tx-FUSEDEG+1 : tx+T+FUSEDEG-1]
                //   iter q0=FUSEDEG-1 computes [tx           : tx+T]
                //   where we have considered the slice of the global arrays.
                // But FUSEDEG might not evenly divide ITER, so we need to
                //   compute UB.
                int32_t UB = MIN(qq+FUSEDEG, ITER) - qq;
                for(int32_t q0 = 0; q0 < UB; q0++) {
                    int32_t q = UB - 1 - q0; // q = UB-1 ... 0
                    
                    // 1. FILL IN THE CODE HERE:
                    //    Hints:
                    //      1. you need a loop going from `x0 = MAX(0,tx-q)` to `x0 < MIN(tx+T+q, DIM_X)`
                    //         so that you only compute the elements corresponding to global
                    //         array slice [0...DIM_X-1]
                    //      2. 1st iteration uses `tile[0][..]` for input and `tile[1][..] for result
                    //         2nd iteration uses `tile[1][..]` for input and `tile[0][..] for result
                    //         3rd iteration uses `tile[0][..]` for input and `tile[1][..] for result
                    //         and so on
                    //      3. having `x0` the loop index, you need to figure out
                    //         how to index in the second dimension of the input 
                    //         and result `tile` array: you probably have to subtract
                    //         `tx` from `x0`, so that you work within the time, and
                    //         you probably need to add `FUSEDEG` because `tile[..][FUSEDEG]`
                    //         semantically corresponds to index `0` of the more intuitive
                    //         declaration `[-FUSEDEG : T+FUSEDEG]`
                }
                // copy back from `tile` array to global result!
                for(int32_t x = 0; x < T; x++) {
                    out[tx+x] = tile[UB%2][FUSEDEG + x];
                }
            } // END #pragma omp for
        
        } // END #pragma omp parallel

        { // double buffering for the outer loop
            NUM* tmp;  tmp = inp;
            inp = out; out = tmp;
        }
    }

    // copy the result in out1 if neccessary
    int32_t num_big_iter = (ITER + FUSEDEG - 1) / FUSEDEG;
    if( (num_big_iter % 2) == 1 ) { 
        #pragma omp parallel for default(shared) schedule(static)
        for(int i=0; i<DIM_X; i++) {
            out1[i] = inp[i];
        }
    }
    free(out20);
}

template<class NUM, int T>
void runStencils(NUM* inp) {
    NUM *out_bf, *out_tf;

    if(DIM_X % T != 0) {
        printf("Error: Tile sizes do not evenly divide DIM_X!\n");
        exit(1);
    }

    {   // allocate space
        const uint32_t DIM_PAD = DIM_X+2*PAD;
        out_bf = (NUM*)malloc(DIM_PAD*sizeof(NUM));
        out_tf = (NUM*)malloc(DIM_PAD*sizeof(NUM));
    }
    // just to amortize thread-spawning overhead!
    
    { // breadth first
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            breadthFirst<NUM>(inp+PAD, out_bf+PAD);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Breadth First runs in: %lu microsecs\n", elapsed);  
    }

    { // tiled fused
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            tiledFused<NUM, T>(inp+PAD, out_tf+PAD);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Tiled   Fused runs in: %lu microsecs\n", elapsed);

        validate<NUM>(DIM_X, out_bf+PAD, out_tf+PAD);
    }

    free(out_bf); free(out_tf);
}

// g++ -fopenmp -O3 -ftree-vectorizer-verbose=1
int main(int argc, char** argv) {
    printf("DIMS: %d\n\n", DIM_X);
    const uint32_t DIM_PAD = DIM_X+2*PAD;

    { // running floats
        float* inp0 = (float*)malloc(DIM_PAD*sizeof(float));
        for(uint32_t i=0; i<DIM_PAD; i++) {
            inp0[i] = rand() / (float)RAND_MAX;
        }
        float* inp = inp0 + PAD;
        printf("RUNNING FLOATs:\n");
        runStencils<float,TILE>(inp0);
        free(inp0);
        printf("\n\n");
    }

    { // running doubles
        double* inp0 = (double*)malloc(DIM_PAD*sizeof(double));
        for(uint32_t i=0; i<DIM_PAD; i++) {
            inp0[i] = rand() / (double)RAND_MAX;
        }
        printf("RUNNING DOUBLEs:\n");  
        double* inp = inp0 + PAD;
        runStencils<double,TILE>(inp0);
        free(inp0);
        printf("\n\n");
    }

    { // running uint8_t
        uint8_t* inp0 = (uint8_t*)malloc(DIM_PAD*sizeof(uint8_t));
        for(uint32_t i=0; i<DIM_PAD; i++) {
            inp0[i] = rand() % 64;
        }
        uint8_t* inp = inp0 + PAD;
        printf("RUNNING UINT8:\n");
        runStencils<uint8_t,TILE>(inp0);
        free(inp0);
        printf("\n\n");
    }
}

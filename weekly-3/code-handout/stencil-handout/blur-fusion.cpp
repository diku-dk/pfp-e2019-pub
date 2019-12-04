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
#ifndef DIM_Y
#define DIM_Y (16*2048) //2048
#endif

#ifndef DIM_X
#define DIM_X (3072) //3072
#endif

#ifndef T2D
#define T2D  128
#endif

#ifndef T1D
#define T1D  128
#endif
/************************/

#define RUNS    15
#define EPS     0.000001

#define IND(arr,y,x)    (arr[(y)*(DIM_X)+(x)])

template<class NUM>
void validate(const int Y, const int X, const int rowlen, NUM* a1, NUM* a2) {
    for(int32_t i=0; i<Y; i++) {
      for(int32_t j=0; j<X; j++) {
        const int32_t ind = i*rowlen + j;
        NUM v1 = a1[ind];
        NUM v2 = a2[ind];
        if(fabs((float)(v1-v2)) > EPS) {
            std::cout<<"INVALID at index: "<<i<<", v1: "<<v1<<" v2: "<<v2<<"\n";
            exit(1);
        }
      }
    }
    printf("VALIDATES!!!\n");
}

inline int32_t modX(const int32_t ind) { return ((ind+DIM_X) % DIM_X); }
inline int32_t modY(const int32_t ind) { return ((ind+DIM_Y) % DIM_Y); }

template<class NUM>
void breadthFirst(NUM* in, NUM* blurx, NUM* out) {
    #pragma omp parallel for default(shared) schedule(static) //if(DIM_Y>4) 
    for(int32_t y=0; y<DIM_Y; y++) {
        for(int32_t x=0; x<DIM_X; x++) {
            NUM inm1, inp1, inc;
            inc  = IND(in, y, x  );
            inm1 = IND(in, y, modX(x-1));
            inp1 = IND(in, y, modX(x+1));
            IND(blurx, y, x) = (inm1 + inc + inp1) / 3;
        }
    }
    #pragma omp parallel for default(shared) schedule(static) //if(DIM_Y>4) 
    for(int32_t y=0; y<DIM_Y; y++) {
        for(int32_t x=0; x<DIM_X; x++) {
            NUM blurm1, blurp1, blurc;
            blurc  = IND(blurx, y,   x);
            blurm1 = IND(blurx, modY(y-1), x);
            blurp1 = IND(blurx, modY(y+1), x);
            IND(out, y, x) = (blurm1 + blurc + blurp1) / 3;
        }
    }
}

template<class NUM>
void fullyFused(NUM* in, NUM* out) {
  #pragma omp parallel for default(shared) schedule(static)
  for(int32_t y=0; y<DIM_Y; y++) {
      NUM blurx[3];
      for(int32_t x=0; x<DIM_X; x++) {
        for(int32_t i=0; i<3; i++) {
            int32_t ind_y = modY(y-1+i);
            NUM inc  = IND(in, ind_y, x);
            NUM inm1 = IND(in, ind_y, modX(x-1));
            NUM inp1 = IND(in, ind_y, modX(x+1));
            blurx[i] = (inm1 + inc + inp1) / 3;
        }
        // compute out
        IND(out,y,x) = (blurx[0] + blurx[1] + blurx[2]) / 3;
      }
  }
}

template<class NUM>
void slideWindow(NUM* in, NUM* out) {
    NUM blurx[3][DIM_X];
    for(int32_t y=-1; y<DIM_Y+1; y++) {
        // computation of blurx
        int32_t ind_y = modY(y);
        #pragma omp parallel for default(shared) schedule(static) 
        for(int32_t x=0; x<DIM_X; x++) {
            NUM inm1, inp1, inc;
            inc  = IND(in, ind_y, modX(x)  );
            inm1 = IND(in, ind_y, modX(x-1));
            inp1 = IND(in, ind_y, modX(x+1));
        
            blurx[(y+1)%3][x] = (inm1 + inc + inp1) / 3;
        }
        
        if(y < 1) continue;

        // computation of out
        #pragma omp parallel for default(shared) schedule(static) 
        for(int32_t x=0; x<DIM_X; x++) {
            NUM blurm1, blurp1, blurc;
            blurc  = blurx[(y+1)%3][x];
            blurm1 = blurx[(y+2)%3][x];
            blurp1 = blurx[(y+3)%3][x];
            IND(out, y-1, x) = (blurm1 + blurc + blurp1) / 3;
        }
        // end 
    }
}


/**
 * Weekly 3, Task2.a: 
 *   Implement tile-fusion scheduling for the blur example.
 *   Template parameter NUM is the array-element type, and
 *     T is the tile size (32 in lecture slides).
 *   Arguments:
 *     `in`  is the input  2D array
 *     `out` is the result 2D array
 *
 *   The input to stencil is semantically a 2d array: DIM_Y x DIM_X
 *     but, of course, the representation is a flat 1d array.
 *   To index element `(y,x)` of `arr` you can use the macro
 *     `IND(arr,y,x)`
 *   The stencil implementation uses a wrap-around technique
 *     for boundary cells: see how `modX` and `modY` are used;
 *     please note that you have to use this explicitly; the `IND`
 *     macro will not do that for you.
 *   Please note that the tile `T` is known to evenly divide `DIM_Y`!
 *
 *   ToDo: the code structure and OMP parallelization is already
 *         provided to you. Your task is to fill in the missing
 *         parts of the code (1. and 2.). The left-hand side of
 *         the final assignment is also provided, i.e.,
 *         `1. blurx[y+1][x] = ..` and `2. IND(out, ind_y, ind_x) = ...`
 */
template<class NUM, int32_t T>
void tiledFused(NUM* in, NUM* out) {
  #pragma omp parallel default(shared) 
  {
    NUM blurx[T+2][T];
    #pragma omp for schedule(static)
    for(int32_t ty=0; ty<DIM_Y; ty+=T) {
      for(int32_t tx=0; tx<DIM_X; tx+=T) {
          // computation of `blurx`
          for(int32_t y=-1; y<T+1; y++) { 
            for(int32_t x=0; x<T; x++) {
                // 1. fill in the code here
                NUM dummy = 3;
                blurx[y+1][x] = dummy;
            }
          }
   
          // computation of `out` based on the previously computed `blurx`
          for(int32_t y=1; y<T+1; y++) {
            for(int32_t x=0; x<T; x++) {
                // 2. fill in the code here 
                int32_t ind_y = ty+y-1, ind_x = tx+x;
                NUM dummy = 3;
                IND(out, ind_y, ind_x) = dummy;
            }
          }
          // end 
      }
    }
  }
}

/**
 * Weekly 3, Task2.b: 
 *   Implement tile-sliding-window scheduling for the blur example.
 *   Template parameter NUM is the array-element type, and
 *     T is the tile size (8 in lecture slides).
 *   Arguments are as in previous function (Weekly 3, Task2.a).
 *
 *   Please note that the tile `T` is known to evenly divide `DIM_Y`!
 *
 *   ToDo: the code structure and OMP parallelization is already
 *         provided to you. Your task is to fill in the missing
 *         parts of the code (1. and 2.). The left-hand side of
 *         the final assignment is also provided, i.e.,
 *         `1. blurx[(y+2)%3][x] = ..` and `2. IND(out, ty+y, x) = ...`
 */ 
template<class NUM, int32_t T>
void tiledWindow(NUM* in, NUM* out) {
  #pragma omp parallel default(shared) 
  {
    NUM blurx[3][DIM_X];
    #pragma omp for schedule(static)
    for(int32_t ty=0; ty<DIM_Y; ty+=T) {
        for(int32_t y=-2; y<T; y++) {
            // computation of `blurx`
            for(int32_t x=0; x<DIM_X; x++) {
                // 1. fill in the code here
                NUM dummy = 3;
                blurx[(y+2)%3][x] = dummy;
            }
        
            if(y < 0) continue;

            // computation of `out` based on previously computed `blurx`
            for(int32_t x=0; x<DIM_X; x++) {
                // 2. fill in the code here
                NUM dummy = 3;
                IND(out, ty+y, x) = dummy;
            }
        }  // end 
    }
  }
}

template<class NUM, int T2, int T1>
void runStencils(NUM* inp) {
    NUM *blur, *out_bf, *out_ff, *out_sw, *out_tf, *out_twf;

    if((DIM_Y % T2 != 0) || (DIM_Y % T1 != 0)) {
        printf("Error: Tile sizes do not evenly divide DIM_Y!\n");
        exit(1);
    }

    {   // allocate space
        const uint32_t DIM_FLAT_EXP = DIM_Y*DIM_X;

        blur    = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
        out_bf  = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
        out_ff  = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
        out_sw  = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
        out_tf  = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
        out_twf = (NUM*)malloc(DIM_FLAT_EXP*sizeof(NUM));
    }
    // just to amortize thread-spawning overhead!
    
    { // breadth first
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            breadthFirst<NUM>(inp, blur, out_bf);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Breadth First runs in: %lu microsecs\n", elapsed);  
    }

    { // fully fused
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            fullyFused<NUM>(inp, out_ff);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Fully Fused   runs in: %lu microsecs\n", elapsed);

        validate<NUM>(DIM_Y, DIM_X, DIM_X, out_bf, out_ff);
    }


    { // sliding window
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            slideWindow<NUM>(inp, out_sw);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Slide Window  runs in: %lu microsecs\n", elapsed);

        validate<NUM>(DIM_Y, DIM_X, DIM_X, out_bf, out_sw);
    }


    { // tiled fused
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            tiledFused<NUM, T2>(inp, out_tf);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("Tiled   Fused runs in: %lu microsecs\n", elapsed);

        validate<NUM>(DIM_Y, DIM_X, DIM_X, out_bf, out_tf);
    }

    { // tiled window fused
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        for(int k=0; k<RUNS; k++) {
            tiledWindow<NUM, T1>(inp, out_twf);
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / RUNS; 
        printf("TiledWinFused runs in: %lu microsecs\n", elapsed);

        validate<NUM>(DIM_Y, DIM_X, DIM_X, out_bf, out_twf);
    }

    free(blur); free(out_bf); free(out_ff); free(out_sw); free(out_tf); free(out_twf);
}

// g++ -fopenmp -O3 -ftree-vectorizer-verbose=1
int main(int argc, char** argv) {
    printf("DIMS: y: %d, x: %d\n\n", DIM_Y, DIM_X);

    { // running floats
        float* inp = (float*)malloc(DIM_X*DIM_Y*sizeof(float));
        for(uint32_t i=0; i<DIM_Y*DIM_X; i++) {
            inp[i] = rand() / (float)RAND_MAX;
        }
        printf("RUNNING FLOATs:\n");
        runStencils<float,T2D,T1D>(inp);
        free(inp);
        printf("\n\n");
    }

    { // running doubles
        double* inp = (double*)malloc(DIM_X*DIM_Y*sizeof(double));
        for(uint32_t i=0; i<DIM_Y*DIM_X; i++) {
            inp[i] = rand() / (double)RAND_MAX;
        }
        printf("RUNNING DOUBLEs:\n");
        runStencils<double,T2D,T1D>(inp);
        free(inp);
        printf("\n\n");
    }

    { // running uint8_t
        uint8_t* inp = (uint8_t*)malloc(DIM_X*DIM_Y*sizeof(uint8_t));
        for(uint32_t i=0; i<DIM_Y*DIM_X; i++) {
            inp[i] = rand() % 64;
        }
        printf("RUNNING UINT8:\n");
        runStencils<uint8_t,T2D,T1D>(inp);
        free(inp);
        printf("\n\n");
    }
}

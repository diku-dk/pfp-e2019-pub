#include <sys/time.h>

// Report the number of microseconds passed since last call to timer()
// (the first call will return a bogus value).
//
// Warning: not thread-safe.
static int timer() {
  static struct timeval t_start, t_end;

  gettimeofday(&t_end, NULL);

  int elapsed =
    (t_end.tv_sec*1000000+t_end.tv_usec) -
    (t_start.tv_sec*1000000+t_start.tv_usec);

  t_start = t_end;

  return elapsed;
}

static int num_runs = 10;

#define TIMEIT(v) for (int num_run = (timer(), 0); num_run < num_runs; num_run++, v = (num_run == num_runs ? timer()/num_runs : 0))

/* foreign3.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdio.h>
#include <stdlib.h>

typedef char i8;
#ifdef _WIN32
typedef __int64 i64;
# define EXPORT extern __declspec (dllexport)
#else
typedef long long i64;
# define EXPORT
#endif

/* To help make sure that argument and result handling doens't
   read or write too far, try to provide functions that allocate
   a structure at the end of a memory page (where the next page is
   likely to be unmapped) */
#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))
# include <stdlib.h>
# include <sys/mman.h>
# include <unistd.h>
# include <inttypes.h>

EXPORT void *malloc_at_boundary(int sz)
{
  intptr_t alloc_size = getpagesize();
  char *p;
  p = mmap(NULL, alloc_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  return p + alloc_size - sz;
}

EXPORT void free_at_boundary(void *p)
{
  intptr_t alloc_size = getpagesize();
  munmap((void *)(((intptr_t)p) & ~(alloc_size-1)), alloc_size);
}
#else
EXPORT void *malloc_at_boundary(int sz)
{
  return malloc(sz);
}
 
EXPORT void free_at_boundary(void *p)
{
  free(p);
}
#endif

#define GEN(ts, init, sum)                                              \
  EXPORT ts f4_get_ ## ts () {                                          \
    ts r = init;                                                        \
    return r;                                                           \
  }                                                                     \
  EXPORT double f4_sum_ ## ts (ts v) {                                  \
    return sum(v);                                                      \
  }                                                                     \
  EXPORT double f4_sum_pre_double_ ## ts (double v0, ts v) {            \
    return v0 + sum(v);                                                 \
  }                                                                     \
  EXPORT double f4_sum_ ## ts ## _post_double (ts v, double v0) {       \
    return v0 + sum(v);                                                 \
  }                                                                     \
  EXPORT double f4_sum_pre_int_ ## ts (int v0, ts v) {                  \
    return (double)v0 + sum(v);                                         \
  }                                                                     \
  EXPORT double f4_sum_ ## ts ## _post_int (ts v, int v0) {             \
    return (double)v0 + sum(v);                                         \
  }                                                                     \
  EXPORT double f4_cb_send_ ## ts (double (*cb)(ts)) {                  \
    ts r = init;                                                        \
    return cb(r) + 1.0;                                                 \
  }                                                                     \
  EXPORT double f4_cb_send_pre_int_ ## ts (double (*cb)(int, ts)) {     \
    ts r = init;                                                        \
    return cb(8, r) + 1.0;                                              \
  }                                                                     \
  EXPORT double f4_cb_send_pre_double_ ## ts (double (*cb)(double, ts)) { \
    ts r = init;                                                        \
    return cb(8.25, r) + 1.0;                                           \
  }                                                                     \
  EXPORT double f4_sum_cb_ ## ts (ts (*cb)()) {                         \
    ts v = cb();                                                        \
    return sum(v);                                                      \
  }

/* For any sane ABI, a struct containing just a primitive type
   is the same as just the primitive type */
#define TO_DOUBLE(x) ((double)(x))
GEN(i8, 11, TO_DOUBLE)
GEN(short, 22, TO_DOUBLE)
GEN(long, 33, TO_DOUBLE)
GEN(int, 44, TO_DOUBLE)
GEN(float, 55.0, TO_DOUBLE)
GEN(double, 66.0, TO_DOUBLE)

#define GEN_2(t1, t2, v1, v2)                                           \
  typedef struct t1 ## _ ## t2 { t1 x; t2 y; } t1 ## _ ## t2;           \
  static double _f4_sum_ ## t1 ## _ ## t2 (t1 ## _ ## t2 v) {           \
    return (double)v.x + (double)v.y;                                   \
  }                                                                     \
  static t1 ## _ ## t2 init_ ## t1 ## _ ## t2 = { v1, v2 };             \
  GEN(t1 ## _ ## t2, init_ ## t1 ## _ ## t2, _f4_sum_ ## t1 ## _ ## t2)

#define GEN_2_SET(t, x)                         \
  GEN_2(t, i8, 1+x, 10)                         \
  GEN_2(t, short, 2+x, 20)                      \
  GEN_2(t, long, 3+x, 30)                       \
  GEN_2(t, i64, 5+x, 50)                        \
  GEN_2(short, t, 6, 60+x)                      \
  GEN_2(long, t, 7, 70+x)                       \
  GEN_2(i64, t, 9, 90+x)                        \
  GEN_2(i8, t, 10, 100+x)

GEN_2_SET(int, 0)
GEN_2_SET(float, 0.5)
GEN_2_SET(double, 0.25)

GEN_2(int, int, 4, 40)
GEN_2(float, float, 4.5, 40.5)
GEN_2(double, double, 4.25, 40.25)

#define GEN_3(t1, t2, t3, v1, v2, v3)                                   \
  typedef struct t1 ## _ ## t2 ## _ ## t3 { t1 x; t2 y; t3 z; } t1 ## _ ## t2 ## _ ## t3; \
  static double _f4_sum_ ## t1 ## _ ## t2 ## _ ## t3 (t1 ## _ ## t2 ## _ ## t3 v) { \
    return (double)v.x + (double)v.y + (double)v.z;                     \
  }                                                                     \
  static t1 ## _ ## t2 ## _ ## t3 init_ ## t1 ## _ ## t2 ## _ ## t3 = { v1, v2, v3 }; \
  GEN(t1 ## _ ## t2 ## _ ## t3, init_ ## t1 ## _ ## t2 ## _ ## t3, _f4_sum_ ## t1 ## _ ## t2 ## _ ## t3)

#define GEN_3_SET(t, x)                           \
  GEN_3(t, i8, int, 1+x, 10, 100)                 \
  GEN_3(t, short, int, 2+x, 20, 200)              \
  GEN_3(t, long, int, 3+x, 30, 300)               \
  GEN_3(t, i64, int, 5+x, 50, 500)                \
  GEN_3(short, t, int, 6, 60+x, 600)              \
  GEN_3(long, t, int, 7, 70+x, 700)               \
  GEN_3(i64, t, int, 9, 90+x, 900)                \
  GEN_3(i8, t, int, 10, 100+x, 1000)

GEN_3_SET(int, 0)
GEN_3_SET(float, 0.5)
GEN_3_SET(double, 0.25)

GEN_3(i8, i8, i8, 4, 38, 127)
GEN_3(short, short, short, 4, 39, 399)
GEN_3(int, int, int, 4, 40, 400)
GEN_3(float, float, float, 4.5, 40.5, 400.5)
GEN_3(double, double, double, 4.25, 40.25, 400.25)

typedef struct i8_i8_i8_i8_i8 { i8 x, y, z, w, q; } i8_i8_i8_i8_i8;
static double _f4_sum_i8_i8_i8_i8_i8 (i8_i8_i8_i8_i8 v) {
  return (double)v.x + (double)v.y + (double)v.z + (double)v.w + (double)v.q;
}
static struct i8_i8_i8_i8_i8 init_i8_i8_i8_i8_i8 = { 1, 2, 3, 4, 5 };
GEN(i8_i8_i8_i8_i8, init_i8_i8_i8_i8_i8, _f4_sum_i8_i8_i8_i8_i8)

typedef struct i8_i8_i8_i8_i8_i8_i8 { i8 x, y, z, w, q, r, s; } i8_i8_i8_i8_i8_i8_i8;
static double _f4_sum_i8_i8_i8_i8_i8_i8_i8 (struct i8_i8_i8_i8_i8_i8_i8 v) {
  return (double)v.x + (double)v.y + (double)v.z + (double)v.w + (double)v.q + (double)v.r + (double)v.s;
}
static struct i8_i8_i8_i8_i8_i8_i8 init_i8_i8_i8_i8_i8_i8_i8 = { 1, 2, 3, 4, 5, 6, 7 };
GEN(i8_i8_i8_i8_i8_i8_i8, init_i8_i8_i8_i8_i8_i8_i8, _f4_sum_i8_i8_i8_i8_i8_i8_i8)

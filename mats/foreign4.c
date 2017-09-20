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
#else
typedef long long i64;
#endif


#define GEN_2(t1, t2, v1, v2)                  \
  struct f4_ ## t1 ## _ ## t2 { t1 x; t2 y; };              \
  struct f4_ ## t1 ## _ ## t2 f4_get_ ## t1 ## _ ## t2 () { \
    struct f4_ ## t1 ## _ ## t2 r = { v1, v2 };             \
    return r;                                               \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 (struct f4_ ## t1 ## _ ## t2 v) {     \
    return (double)v.x + (double)v.y;                                   \
  }                                                                     \
  double f4_sum_pre_double_ ## t1 ## _ ## t2 (double v0, struct f4_ ## t1 ## _ ## t2 v) { \
    return v0 + (double)v.x + (double)v.y;                              \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 ## _post_double (struct f4_ ## t1 ## _ ## t2 v, double v0) { \
    return v0 + (double)v.x + (double)v.y;                              \
  }                                                                     \
  double f4_sum_pre_int_ ## t1 ## _ ## t2 (int v0, struct f4_ ## t1 ## _ ## t2 v) { \
    return (double)v0 + (double)v.x + (double)v.y;                      \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 ## _post_int (struct f4_ ## t1 ## _ ## t2 v, int v0) { \
    return (double)v0 + (double)v.x + (double)v.y;                      \
  }                                                                     \
  double f4_cb_send_ ## t1 ## _ ## t2 (double (*cb)(struct f4_ ## t1 ## _ ## t2)) { \
    struct f4_ ## t1 ## _ ## t2 r = { v1, v2 };                         \
    return cb(r) + 1.0;                                                 \
    }                                                                   \
  double f4_cb_send_pre_int_ ## t1 ## _ ## t2 (double (*cb)(int, struct f4_ ## t1 ## _ ## t2)) { \
    struct f4_ ## t1 ## _ ## t2 r = { v1, v2 };                         \
    return cb(8, r) + 1.0;                                              \
    }                                                                   \
  double f4_cb_send_pre_double_ ## t1 ## _ ## t2 (double (*cb)(double, struct f4_ ## t1 ## _ ## t2)) { \
    struct f4_ ## t1 ## _ ## t2 r = { v1, v2 };                         \
    return cb(8.25, r) + 1.0;                                           \
    }                                                                   \
  double f4_sum_cb_ ## t1 ## _ ## t2 (struct f4_ ## t1 ## _ ## t2 (*cb)()) { \
    struct f4_ ## t1 ## _ ## t2 v = cb();                               \
    return (double)v.x + (double)v.y;                                   \
  }                                                                     

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
  struct f4_ ## t1 ## _ ## t2 ## _ ## t3{ t1 x; t2 y; t3 z; };          \
  struct f4_ ## t1 ## _ ## t2 ## _ ## t3 f4_get_ ## t1 ## _ ## t2 ## _ ## t3 () { \
    struct f4_ ## t1 ## _ ## t2 ## _ ## t3 r = { v1, v2, v3 };          \
    return r;                                                           \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 ## _ ## t3 (struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v) { \
    return (double)v.x + (double)v.y + (double)v.z;                     \
  }                                                                     \
  double f4_sum_pre_double_ ## t1 ## _ ## t2 ## _ ## t3 (double v0, struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v) { \
    return v0 + (double)v.x + (double)v.y + (double)v.z;                \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 ## _ ## t3 ## _post_double (struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v, double v0) { \
    return v0 + (double)v.x + (double)v.y + (double)v.z;                \
  }                                                                     \
  double f4_sum_pre_int_ ## t1 ## _ ## t2 ## _ ## t3 (int v0, struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v) { \
    return (double)v0 + (double)v.x + (double)v.y + (double)v.z;        \
  }                                                                     \
  double f4_sum_ ## t1 ## _ ## t2 ## _ ## t3 ## _post_int (struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v, int v0) { \
    return (double)v0 + (double)v.x + (double)v.y + (double)v.z;        \
  }                                                                     \
  double f4_cb_send_ ## t1 ## _ ## t2 ## _ ## t3 (double (*cb)(struct f4_ ## t1 ## _ ## t2 ## _ ## t3)) { \
    struct f4_ ## t1 ## _ ## t2 ## _ ## t3 r = { v1, v2, v3 };          \
    return cb(r) + 1.0;                                                 \
    }                                                                   \
  double f4_cb_send_pre_int_ ## t1 ## _ ## t2 ## _ ## t3 (double (*cb)(int, struct f4_ ## t1 ## _ ## t2 ## _ ## t3)) { \
    struct f4_ ## t1 ## _ ## t2 ## _ ## t3 r = { v1, v2, v3 };          \
    return cb(8, r) + 1.0;                                              \
    }                                                                   \
  double f4_cb_send_pre_double_ ## t1 ## _ ## t2 ## _ ## t3 (double (*cb)(double, struct f4_ ## t1 ## _ ## t2 ## _ ## t3)) { \
    struct f4_ ## t1 ## _ ## t2 ## _ ## t3 r = { v1, v2, v3 };          \
    return cb(8.25, r) + 1.0;                                           \
    }                                                                   \
  double f4_sum_cb_ ## t1 ## _ ## t2 ## _ ## t3 (struct f4_ ## t1 ## _ ## t2 ## _ ## t3 (*cb)()) { \
    struct f4_ ## t1 ## _ ## t2 ## _ ## t3 v = cb();                    \
    return (double)v.x + (double)v.y + (double)v.z;                     \
  }

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

GEN_3(int, int, int, 4, 40, 400)
GEN_3(float, float, float, 4.5, 40.5, 400.5)
GEN_3(double, double, double, 4.25, 40.25, 400.25)

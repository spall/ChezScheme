/* gc-oce.c
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

#define GCENTRY S_gc_oce
#define ENABLE_OBJECT_COUNTS
#define ENABLE_BACKREFERENCE
#include "gc.c"

static uptr measure(ptr p, int depth, IGEN generation); /* in measure.inc */

static void init_measure_mask(seginfo *si);
static void clear_measured_masks();
static uptr push_measure(ptr p, int depth, IGEN generation, seginfo *si);
static void add_ephemeron_to_pending_measure(ptr pe);
static void add_trigger_ephemerons_to_pending_measure(ptr pe);
static uptr check_ephemeron_measure(ptr pe, IGEN generation);
static uptr check_pending_measure_ephemerons(IGEN generation);

static ptr *measure_stack_start, *measure_stack, *measure_stack_limit;
static seginfo *measured_mask_chain, *nonkey_mask_chain;
static ptr pending_measure_ephemerons;

#define measure_unreached(si, p) \
  (!si->measured_mask \
   || !(si->measured_mask[segment_bitmap_byte(si, p)] & (1 << segment_bitmap_bit(si, p))))

#define measured_mask_bytes (bytes_per_segment >> (log2_ptr_bytes+3))

static void init_measure_mask(seginfo *si) {
  find_room(space_data, 0, typemod, ptr_align(measured_mask_bytes+ptr_bytes), si->measured_mask);
  memset(si->measured_mask, 0, measured_mask_bytes);
  *(ptr *)(si->measured_mask + measured_mask_bytes) = measured_mask_chain;
  measured_mask_chain = si;
}

static void init_nonkey_mask(seginfo *si) {
  find_room(space_data, 0, typemod, ptr_align(measured_mask_bytes+ptr_bytes), si->nonkey_mask);
  memset(si->nonkey_mask, 0, measured_mask_bytes);
  *(ptr *)(si->nonkey_mask + measured_mask_bytes) = nonkey_mask_chain;
  nonkey_mask_chain = si;
}

static void clear_measured_masks()
{
  while (measured_mask_chain) {
    octet *mask = measured_mask_chain->measured_mask;
    measured_mask_chain->measured_mask = NULL;
    measured_mask_chain = *(seginfo **)(mask + measured_mask_bytes);
  }
  while (nonkey_mask_chain) {
    octet *mask = nonkey_mask_chain->nonkey_mask;
    nonkey_mask_chain->nonkey_mask = NULL;
    nonkey_mask_chain = *(seginfo **)(mask + measured_mask_bytes);
  }
}

#define measure_mask_set(mm, si, p) \
  mm[segment_bitmap_byte(si, p)] |= (1 << segment_bitmap_bit(si, p))
#define measure_mask_unset(mm, si, p) \
  mm[segment_bitmap_byte(si, p)] -= (1 << segment_bitmap_bit(si, p))

static uptr push_measure(ptr p, int depth, IGEN generation, seginfo *si)
{
  if (!si->measured_mask)
    init_measure_mask(si);
  measure_mask_set(si->measured_mask, si, p);

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = NULL;
  }

  if (depth < 10)
    return measure(p, depth, generation);
  else {
    if (measure_stack == measure_stack_limit) {
      uptr sz = ptr_bytes * (measure_stack_limit - measure_stack_start);
      uptr new_sz = 2*sz;
      ptr new_measure_stack;
      printf("grow %ld\n", new_sz);
      find_room(space_data, 0, typemod, ptr_align(new_sz), new_measure_stack);
      memcpy(new_measure_stack, measure_stack_start, sz);
      measure_stack_start = (ptr *)new_measure_stack;
      measure_stack_limit = (ptr *)((uptr)new_measure_stack + new_sz);
      measure_stack = (ptr *)((uptr)new_measure_stack + sz);
    }

    *(measure_stack++) = p;

    return 0;
  }
}

static void add_ephemeron_to_pending_measure(ptr pe) {
  EPHEMERONNEXT(pe) = pending_measure_ephemerons;
  pending_measure_ephemerons = pe;
}

static void add_trigger_ephemerons_to_pending_measure(ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONTRIGGERNEXT(pe);
  while (next_pe != NULL) {
    last_pe = next_pe;
    next_pe = EPHEMERONTRIGGERNEXT(next_pe);
  }
  EPHEMERONTRIGGERNEXT(last_pe) = pending_ephemerons;
  pending_ephemerons = pe;
}

static uptr check_ephemeron_measure(ptr pe, IGEN generation) {
  ptr p;
  seginfo *si;

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && (si->generation < generation)) {
    if (measure_unreached(si, p)
        || (si->nonkey_mask
            && (si->nonkey_mask[segment_bitmap_byte(si, p)] & (1 << segment_bitmap_bit(si, p))))) {
      /* Not reached, so far; install as trigger */
      EPHEMERONTRIGGERNEXT(pe) = si->trigger_ephemerons;
      si->trigger_ephemerons = pe;
      if (!si->measured_mask)
        init_measure_mask(si); /* so triggers are cleared at end */
      return 0;
    }
  }

  p = Scdr(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && (si->generation < generation)
      && measure_unreached(si, p))
    return push_measure(p, 0, generation, si);

  return 0;
}

static uptr check_pending_measure_ephemerons(IGEN generation) {
  ptr pe, next_pe;
  uptr total = 0;

  pe = pending_measure_ephemerons;
  pending_measure_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONNEXT(pe);
    total += check_ephemeron_measure(pe, generation);
    pe = next_pe;
  }

  return total;
}

#include "measure.inc"

uptr gc_measure_one(ptr p, IGEN generation) {
  uptr total;
  
  total = measure(p, 0, generation);

  while (1) {
    while (measure_stack > measure_stack_start)
      total += measure(*(--measure_stack), 0, generation);

    if (!pending_measure_ephemerons)
      break;
    total += check_pending_measure_ephemerons(generation);
  }

  return total;
}

ptr S_count_size_increments(ptr ls, IGEN generation) {
  ptr l, totals = Snil, totals_prev = NULL;
  uptr init_stack_len = 1024;

  tc_mutex_acquire();

  find_room(space_data, 0, typemod, init_stack_len, measure_stack_start);
  measure_stack = (ptr *)measure_stack_start;
  measure_stack_limit = (ptr *)((uptr)measure_stack_start + init_stack_len);

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));

      if (!si->measured_mask)
        init_measure_mask(si);
      measure_mask_set(si->measured_mask, si, p);

      if (!si->nonkey_mask)
        init_nonkey_mask(si);
      measure_mask_set(si->nonkey_mask, si, p);
    }
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    uptr total;

    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      measure_mask_unset(si->nonkey_mask, si, p);
      total = gc_measure_one(p, generation);
    } else
      total = 0;

    p = Scons(FIX(total), Snil);
    if (totals_prev)
      Scdr(totals_prev) = p;
    else
      totals = p;
    totals_prev = p;
  }

  clear_measured_masks();

  tc_mutex_release();

  return totals;
}

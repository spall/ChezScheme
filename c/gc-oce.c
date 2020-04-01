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

static void measure(ptr p); /* in measure.inc */

static void init_measure_mask(seginfo *si);
static void clear_measured_masks();
static void push_measure(ptr p);
static void add_ephemeron_to_pending_measure(ptr pe);
static void add_trigger_ephemerons_to_pending_measure(ptr pe);
static void check_ephemeron_measure(ptr pe);
static void check_pending_measure_ephemerons();

static uptr measure_total; /* updated by `measure` */

static IGEN measure_generation;
static ptr *measure_stack_start, *measure_stack, *measure_stack_limit;
static seginfo *measured_mask_chain, *nonkey_mask_chain;
static ptr pending_measure_ephemerons;

#define measure_unreached(si, p) \
  (!si->measured_mask \
   || !(si->measured_mask[segment_bitmap_byte(p)] & (1 << segment_bitmap_bit(p))))

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
    measured_mask_chain->trigger_ephemerons = NULL;
    measured_mask_chain = *(seginfo **)(mask + measured_mask_bytes);
  }
  while (nonkey_mask_chain) {
    octet *mask = nonkey_mask_chain->nonkey_mask;
    nonkey_mask_chain->nonkey_mask = NULL;
    nonkey_mask_chain = *(seginfo **)(mask + measured_mask_bytes);
  }
}

#define measure_mask_set(mm, si, p) \
  mm[segment_bitmap_byte(p)] |= (1 << segment_bitmap_bit(p))
#define measure_mask_unset(mm, si, p) \
  mm[segment_bitmap_byte(p)] -= (1 << segment_bitmap_bit(p))

static void push_measure(ptr p)
{
  seginfo *si = MaybeSegInfo(ptr_get_segment(p));

  if (!si)
    return;

  if (si->generation > measure_generation)
    return;
  else {
    uptr byte = segment_bitmap_byte(p);
    uptr bit = 1 << segment_bitmap_bit(p);

    if (!si->measured_mask)
      init_measure_mask(si);
    else if (si->measured_mask[byte] & bit)
      return;

    si->measured_mask[byte] |= bit;
  }

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = NULL;
  }

  if (measure_stack == measure_stack_limit) {
    uptr sz = ptr_bytes * (measure_stack_limit - measure_stack_start);
    uptr new_sz = 2*sz;
    ptr new_measure_stack;
    find_room(space_data, 0, typemod, ptr_align(new_sz), new_measure_stack);
    memcpy(new_measure_stack, measure_stack_start, sz);
    measure_stack_start = (ptr *)new_measure_stack;
    measure_stack_limit = (ptr *)((uptr)new_measure_stack + new_sz);
    measure_stack = (ptr *)((uptr)new_measure_stack + sz);
  }
  
  *(measure_stack++) = p;
}

static void add_ephemeron_to_pending_measure(ptr pe) {
  EPHEMERONNEXT(pe) = pending_measure_ephemerons;
  pending_measure_ephemerons = pe;
}

static void add_trigger_ephemerons_to_pending_measure(ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONNEXT(pe);
  while (next_pe != NULL) {
    last_pe = next_pe;
    next_pe = EPHEMERONNEXT(next_pe);
  }
  EPHEMERONNEXT(last_pe) = pending_measure_ephemerons;
  pending_measure_ephemerons = pe;
}

static void check_ephemeron_measure(ptr pe) {
  ptr p;
  seginfo *si;

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && (si->generation < measure_generation)) {
    if (measure_unreached(si, p)
        || (si->nonkey_mask
            && (si->nonkey_mask[segment_bitmap_byte(p)] & (1 << segment_bitmap_bit(p))))) {
      /* Not reached, so far; install as trigger */
      EPHEMERONNEXT(pe) = si->trigger_ephemerons;
      si->trigger_ephemerons = pe;
      if (!si->measured_mask)
        init_measure_mask(si); /* so triggers are cleared at end */
      return;
    }
  }
  
  p = Scdr(pe);
  if (!IMMEDIATE(p))
    push_measure(p);
}

static void check_pending_measure_ephemerons() {
  ptr pe, next_pe;

  pe = pending_measure_ephemerons;
  pending_measure_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron_measure(pe);
    pe = next_pe;
  }
}

#include "measure.inc"

void gc_measure_one(ptr p) {
  measure(p);

  while (1) {
    while (measure_stack > measure_stack_start)
      measure(*(--measure_stack));

    if (!pending_measure_ephemerons)
      break;
    check_pending_measure_ephemerons();
  }
}

ptr S_count_size_increments(ptr ls, IGEN generation) {
  ptr l, totals = Snil, totals_prev = NULL;
  uptr init_stack_len = 1024;

  tc_mutex_acquire();

  measure_generation = generation;
  
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

    measure_total = 0;

    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      measure_mask_unset(si->nonkey_mask, si, p);
      gc_measure_one(p);
    }

    p = Scons(FIX(measure_total), Snil);
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

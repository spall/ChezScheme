/* vfasl.c
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

#include "system.h"

#define HEADER_INITIAL_FIELDS   10
#define HEADER_PER_SPACE_FIELDS 2

/* vfasl format, where the fixed-size header determines the rest of the
   size:
     _
    /   [uptr: offset for result]
   /
  |     [uptr: symbol count = N_sym]
  |     [uptr: symbol reference count = N_symref]
h |     [uptr: closure count = N_closure]
e |     [uptr: code count = N_code]
a |     [uptr: rtd count = N_rtd]
a |     [uptr: rtd reference count = N_rtdref]
d |     [uptr: singleton reference count = N_singleton]
e |     [uptr: tlc count = N_tlc]
r |
  |     [uptr: length for space 0]
  |     ...
  |     [uptr: length for space N_space]
  |
  |     [uptr: bit set per space to indicate RLE-compressed]
  |
  |     [uptr: bitmap length for space 0]
   \    ...
    \_  [uptr: bitmap length for space N_space]

        [uptr: offset for symbol reference 0]
        ...
        [uptr: offset for symbol reference N_symref]

        [uptr: offset for closure 0]
        ...
        [uptr: offset for closure N_closure]

        [uptr: offset for code 0]
        ...
        [uptr: offset for code N_count]

        [uptr: offset for rtd 0]
        ...
        [uptr: offset for rtd N_rtd]

        [uptr: offset for rtd reference 0]
        ...
        [uptr: offset for rtd reference N_rtdref]

        [uptr: offset for singleton reference 0]
        ...
        [uptr: offset for rtd singleton N_singleton]

        [uptr: offset for tlc 0]
        ...
        [uptr: offset for tlc N_tlc]

        [MAYBE padding to get alignment]

        [data for space 0]
        ...
        [data for space N_space]

        [bitmap for space 0]
        ...
        [bitmap for space N_space]
*/

typedef struct vfasl_chunk {
  ptr bytes;
  uptr length;
  uptr used;
  uptr swept;
  struct vfasl_chunk *next;
} vfasl_chunk;

struct vfasl_count_and_chunk {
  uptr total_bytes;
  vfasl_chunk *first;
  uptr *ptr_bitmap; /* bit set for references into this space */
};

typedef struct hash_entry {
  ptr key, value;
} hash_entry;

typedef struct vfasl_hash_table {
  uptr count;
  uptr size;
  hash_entry *entries;
} vfasl_hash_table;

typedef struct vfasl_info {
  ptr base_addr; /* address to make relocations relative to */

  uptr sym_count;

  uptr symref_count;
  uptr *symrefs;

  uptr closure_count;
  uptr *closures;

  uptr code_count;
  uptr *codes;

  ptr base_rtd; /* track replacement base_rtd to recognize other rtds */
  uptr rtd_count;
  uptr *rtds;

  uptr rtdref_count;
  uptr *rtdrefs;

  uptr singletonref_count;
  uptr *singletonrefs;

  uptr tlc_count;
  uptr *tlcs;

  struct vfasl_count_and_chunk spaces[space_empty];

  vfasl_hash_table *graph;
} vfasl_info;

/* Bitmap is 1 bit per pointer, with 8*sizeof(uptr) bits per
   uptr in the bitmap */
#define log2_ptr_bits_per_byte (log2_ptr_bytes + 3)
#define log2_ptr_bits_per_uptr (log2_ptr_bits_per_byte + log2_ptr_bytes)

#define ptr_add(p, n) ((ptr)((uptr)(p) + (n)))
#define ptr_subtract(p, n) ((ptr)((uptr)(p) - (n)))
#define ptr_diff(p, q) ((uptr)(p) - (uptr)(q))

static ptr vfasl_copy_all(vfasl_info *vfi, ptr v);

static ptr copy(vfasl_info *vfi, ptr pp, seginfo *si);
static void sweep_ptrs(vfasl_info *vfi, ptr *pp, iptr n);
static uptr sweep_code_object(vfasl_info *vfi, ptr co);
static uptr sweep_record(vfasl_info *vfi, ptr co);
static uptr sweep(vfasl_info *vfi, ptr p);

static void relink_code(ptr co, ptr sym_base, ptr *spaces, uptr *space_offsets);

static void vfasl_relocate(vfasl_info *vfi, ptr *ppp);
static ptr vfasl_relocate_help(vfasl_info *vfi, ptr pp);
static ptr vfasl_find_room(vfasl_info *vfi, ISPC s, ITYPE t, iptr n);
static void vfasl_register_rtd(vfasl_info *vfi, ptr p);
static void vfasl_register_rtd_reference(vfasl_info *vfi, ptr pp);
static void vfasl_register_tlc(vfasl_info *vfi, ptr p);
static void vfasl_register_symbol_reference(vfasl_info *vfi, ptr *pp, ptr p);
static void vfasl_register_singleton_reference(vfasl_info *vfi, ptr *pp, int which);
static void vfasl_register_forward(vfasl_info *vfi, ptr pp, ptr p);
static ptr vfasl_lookup_forward(vfasl_info *vfi, ptr p);

static void fasl_init_entry_tables();

static int detect_singleton(ptr p);
static ptr lookup_singleton(int which);

static vfasl_hash_table *make_vfasl_hash_table();
static void free_vfasl_hash_table(vfasl_hash_table *ht);
static void vfasl_hash_table_set(vfasl_hash_table *ht, ptr key, ptr value);
static ptr vfasl_hash_table_ref(vfasl_hash_table *ht, ptr key);

static void sort_offsets(uptr *p, uptr len);

#define vfasl_fail(vfi, what) S_error("vfasl", "cannot encode " what)

#define print_stats(args) /* printf args */

/* must have mutex if target_generation is not 0 */
ptr S_vfasl(ptr bv, void *stream, iptr input_len, int target_generation)
{
  ptr tc = get_thread_context();
  ptr p, base_addr;
  uptr tables_len, pre_data_padding, bitmaps_len;
  uptr *q, v_off;
  uptr sym_count, symref_count, closure_count, code_count, rtd_count;
  uptr rtdref_count, singletonref_count, tlc_count;
  uptr *symrefs, *closures, *codes, *rtds;
  uptr *rtdrefs, *singletonrefs, *tlcs;
  uptr spaces_compressed, *space_lengths, *space_bitmap_lengths;
  ISPC s;
  uptr space_offsets[space_empty+1];
  ptr spaces[space_empty];
  ptr header_buf[HEADER_INITIAL_FIELDS + (HEADER_PER_SPACE_FIELDS * space_empty)], tables;
  iptr used_len;

  used_len = sizeof(header_buf);
  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  if (bv)
    p = &BVIT(bv, 0);
  else {
    if (S_fasl_stream_read(stream, (octet*)header_buf, sizeof(header_buf)) < 0)
      S_error("fasl-read", "input truncated");
    p = header_buf;
  }

  q = (uptr *)p;
  v_off = *(q++);
  sym_count = *(q++);
  symref_count = *(q++);
  closure_count = *(q++);
  code_count = *(q++);
  rtd_count = *(q++);
  rtdref_count = *(q++);
  singletonref_count = *(q++);
  tlc_count = *(q++);

  print_stats(("\n"
               "SIZE     %ld\n"
               "sym      %ld\n"
               "symref   %ld\n"
               "closure  %ld\n"
               "code     %ld\n"
               "rtd      %ld\n"
               "rtdref   %ld\n"
               "singletn %ld\n"
               "tlc      %ld\n",
               input_len,
               sym_count, symref_count,
               closure_count, code_count,
               rtd_count, rtdref_count,
               singletonref_count, tlc_count));

  space_lengths = q;
  q += space_empty;

  spaces_compressed = *(q++);

  space_bitmap_lengths = q;
  q += space_empty;

  tables_len = sizeof(uptr) * ((symref_count + closure_count + code_count
                                + rtd_count + rtdref_count + singletonref_count
                                + tlc_count));
  used_len += tables_len;
  pre_data_padding = ptr_align(used_len) - used_len;
  tables_len += pre_data_padding;
  used_len += pre_data_padding;

  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  if (!bv) {
    thread_find_room(tc, typemod, ptr_align(tables_len), tables);

    if (S_fasl_stream_read(stream, tables, tables_len) < 0)
      S_error("fasl-read", "input truncated");

    q = (uptr *)tables;
  }

  symrefs = q;
  q += symref_count;

  closures = q;
  q += closure_count;

  codes = q;
  q += code_count;

  rtds = q;
  q += rtd_count;

  rtdrefs = q;
  q += rtdref_count;

  singletonrefs = q;
  q += singletonref_count;

  tlcs = q;
  q += tlc_count;

  p = q;
  p = ptr_add(p, pre_data_padding);

  base_addr = p;
  
  for (s = 0; s < space_empty; s++) {
    space_offsets[s] = ptr_diff(p, base_addr);
    if (space_lengths[s] != 0) {
      used_len += space_lengths[s];
      if (used_len > input_len)
        S_error("fasl-read", "input length mismatch");
      print_stats(("%d     %ld\n", s, space_lengths[s]));

      if (target_generation > 0) {
        find_room(s, target_generation, typemod, space_lengths[s], spaces[s]);
      } else if ((s == space_weakpair) || (s == space_ephemeron)) {
        tc_mutex_acquire()
        find_room(s, target_generation, typemod, space_lengths[s], spaces[s]);
        tc_mutex_release()
      } else {
        thread_find_room(tc, typemod, space_lengths[s], spaces[s]);
      }
      if (bv)
        memcpy(spaces[s], p, space_lengths[s]);
      else {
        if (S_fasl_stream_read(stream, spaces[s], space_lengths[s]) < 0)
          S_error("fasl-read", "input truncated");
      }
      p = ptr_add(p, space_lengths[s]);
    }
  }
  space_offsets[space_empty] = ptr_diff(p, base_addr);

  bitmaps_len = 0;
  for (s = 0; s < space_empty; s++) {
    if (space_bitmap_lengths[s]) {
      print_stats(("[%d]     %ld\n", s, space_bitmap_lengths[s]));
    }
    bitmaps_len += space_bitmap_lengths[s];
  }

  used_len += bitmaps_len;
  if (used_len != input_len)
    S_error("fasl-read", "input length mismatch");

  if (!bv) {
    thread_find_room(tc, typemod, ptr_align(bitmaps_len), p);
    if (S_fasl_stream_read(stream, p, bitmaps_len) < 0)
      S_error("fasl-read", "input truncated");
  }

  /* We have to convert an offset relative to the start of data in the
     vfasl format to an offset relative to an individual space, at
     least for target generations other than 0. Rely on the fact that
     the spaces and the references are both sorted. */
#define SPACE_OFFSET_DECLS \
  ISPC s2 = 0; \
  uptr offset2 = space_offsets[s2]; \
  uptr next_offset2 = space_offsets[s2+1]
#define INC_SPACE_OFFSET(off) \
  do { \
    while ((off) >= next_offset2) { \
      s2++;                                \
      offset2 = next_offset2;              \
      next_offset2 = space_offsets[s2+1];  \
    } \
  } while (0)
#define SPACE_PTR(off) ptr_add(spaces[s2], (off) - offset2)

  for (s = 0; s < space_empty; s++) {
    if (space_bitmap_lengths[s] != 0) {
      SPACE_OFFSET_DECLS;
      uptr delta = ptr_diff(spaces[s], space_offsets[s]);
      uptr p_off = 0;
      uptr *bm;
      ptr ptr_bitmap_end = ptr_add(p, space_bitmap_lengths[s]);
      for (bm = (uptr *)p; bm != ptr_bitmap_end; bm++) {
        uptr m;
        if ((m = *bm) == 0) {
          if (spaces_compressed & (1 << s)) {
            p_off += (sizeof(uptr) * 8 * bm[1]) << log2_ptr_bytes;
            bm++;
          } else
            p_off += (sizeof(uptr) * 8) << log2_ptr_bytes;
        } else {
          uptr p_off2 = p_off;
          while (m) {
            if (!(m & 0xF)) {
              m >>= 4;
              p_off2 += (4 * sizeof(uptr));
            } else if (m & 1) {
              ptr p2;
              INC_SPACE_OFFSET(p_off2);
              p2 = SPACE_PTR(p_off2);
              *(uptr *)p2 += delta;
              p_off2 += sizeof(uptr);
              m >>= 1;
            } else {
              m >>= 1;
              p_off2 += sizeof(uptr);
            }
          }
          p_off += (sizeof(uptr) * 8) << log2_ptr_bytes;
        }
      }
      p = ptr_add(p, space_bitmap_lengths[s]);
    }
  }

  /* Intern symbols */
  {
#define SYM_FROM_SPACE(i) TYPE(ptr_add(spaces[space_symbol], (i) * size_symbol), type_symbol)
    uptr i;

    if (target_generation != 0)
      tc_mutex_acquire()
    
    for (i = 0; i < sym_count; i++) {
      ptr sym, isym;
      sym = SYM_FROM_SPACE(i);

      INITSYMVAL(sym) = sunbound;
      INITSYMCODE(sym,S_G.nonprocedure_code);

      isym = S_intern4(sym);
      if (isym != sym) {
        /* The symbol was already interned, so point to the existing one */
        INITSYMVAL(sym) = isym;
      }
    }

    if (target_generation != 0)
      tc_mutex_release()
  }

  /* Replace symbol references to interned references */
  {
    SPACE_OFFSET_DECLS;
    uptr i;
    for (i = 0; i < symref_count; i++) {
      uptr off, sym_pos;
      ptr p2, sym, val;
      off = symrefs[i];
      INC_SPACE_OFFSET(off);
      p2 = SPACE_PTR(off);
      sym_pos = UNFIX(*(ptr **)p2);
      sym = SYM_FROM_SPACE(sym_pos);
      if ((val = SYMVAL(sym)) != sunbound)
        sym = val;
      *(ptr **)p2 = sym;
    }
  }
  
  /* Intern rtds */
  if (rtd_count) {
    uptr i;
    ptr rtd, rtd_base = ptr_subtract(spaces[space_pure], space_offsets[space_pure]);

    /* first one corresponds to base_rtd */
    rtd = ptr_add(rtd_base, rtds[0]);
    if (RECORDINSTTYPE(rtd) != rtd)
      S_error("vfasl", "broken base-rtd entry");
    RECORDDESCUID(rtd) = S_G.base_rtd;

    for (i = 1; i < rtd_count; i++) {
      ptr rtd, new_rtd, parent_rtd;
      rtd = ptr_add(rtd_base, rtds[i]);

      RECORDINSTTYPE(rtd) = S_G.base_rtd;

      /* fixup type and parent before continuing, relying on parents being earlier in `rtd`s */
      parent_rtd = RECORDDESCPARENT(rtd);
      if (parent_rtd != Sfalse) {
        ptr parent_uid = RECORDDESCUID(parent_rtd);
        if (!Ssymbolp(parent_uid))
          RECORDDESCPARENT(rtd) = parent_uid;
      }

      new_rtd = rtd;
      if (S_fasl_intern_rtd(&new_rtd)) {
        if (new_rtd == rtd) {
          S_error1("vfasl", "incompatible record type ~s", RECORDDESCNAME(rtd));
        } else {
          /* Use the UID field to record already-interned replacement: */
          RECORDDESCUID(rtd) = new_rtd;
        }
      }
    }
  }
  
  /* Replace rtd references to interned references */
  {
    SPACE_OFFSET_DECLS;
    uptr i;
    for (i = 0; i < rtdref_count; i++) {
      uptr off;
      ptr *ref, rtd, uid;
      off = rtdrefs[i];
      INC_SPACE_OFFSET(off);
      ref = SPACE_PTR(off);
      rtd = *ref;
      uid = RECORDDESCUID(rtd);
      if (!Ssymbolp(uid)) {
        /* uid is replacement interned rtd */
        *ref = uid;
      }
    }
  }

  /* Replace references to singletons like "" and #vu8() */
  {
    SPACE_OFFSET_DECLS;
    uptr i;
    for (i = 0; i < singletonref_count; i++) {
      uptr off;
      ptr *ref;
      off = singletonrefs[i];
      INC_SPACE_OFFSET(off);
      ref = SPACE_PTR(off);
      *ref = lookup_singleton(UNFIX(*ref));
    }
  }

  /* Fix code pointers on closures */
  {
    SPACE_OFFSET_DECLS;
    uptr i, code_delta;
    code_delta = (uptr)ptr_subtract(spaces[space_code], space_offsets[space_code]);
    for (i = 0; i < closure_count; i++) {
      uptr off; ptr cl, code;
      off = closures[i];
      INC_SPACE_OFFSET((uptr)UNTYPE(off, type_closure));
      cl = SPACE_PTR(off);
      code = CLOSCODE(cl);
      code = ptr_add(code, code_delta);
      SETCLOSCODE(cl,code);
    }
  }

  /* Fix code via relocations */
  {
    uptr i;
    ptr code_base = ptr_subtract(spaces[space_code], space_offsets[space_code]);
    ptr sym_base = spaces[space_symbol];
    for (i = 0; i < code_count; i++) {
      ptr code = ptr_add(code_base, codes[i]);
      relink_code(code, sym_base, spaces, space_offsets);
    }
  }

  for (s = 0; (uptr)UNTYPE(v_off, TYPEBITS(v_off)) >= space_offsets[s+1]; s++) {
  }

  /* Turn result offeset into a value, unboxing if it's a box (which
     suports a symbol result, for example). */
  {
    ptr v;
    ITYPE t;
    v = ptr_add(spaces[s], (v_off - space_offsets[s]));
    if (((t = TYPEBITS(v)) == type_typed_object)
        && TYPEP(TYPEFIELD(v), mask_box, type_box))
      v = Sunbox(v);

    return v;
  }
}

ptr S_vfasl_to(ptr bv, int target_generation)
{
  return S_vfasl(bv, (ptr)0, Sbytevector_length(bv), target_generation);
}

ptr S_to_vfasl(ptr v)
{
  vfasl_info *vfi;
  ITYPE t;
  ISPC s;
  uptr size, padding, data_size, bitmap_size, pre_bitmap_size;
  ptr bv, p, bitmap_sizes_p;

  fasl_init_entry_tables();

  if (IMMEDIATE(v)
      || ((t = TYPEBITS(v)) == type_symbol)
      || ((t == type_typed_object)
          && TYPEP(TYPEFIELD(v), mask_record, type_record)
          && (TYPEFIELD(v) == v))
      || ((t == type_typed_object)
          && TYPEP(TYPEFIELD(v), mask_box, type_box))) {
    v = Sbox(v);
  }

  vfi = malloc(sizeof(vfasl_info));

  vfi->base_addr = (ptr)0;
  vfi->sym_count = 0;
  vfi->symref_count = 0;
  vfi->symrefs = (ptr)0;
  vfi->closure_count = 0;
  vfi->closures = (ptr)0;
  vfi->code_count = 0;
  vfi->codes = (ptr)0;
  vfi->base_rtd = S_G.base_rtd;
  vfi->rtd_count = 0;
  vfi->rtds = (ptr)0;
  vfi->rtdref_count = 0;
  vfi->rtdrefs = (ptr)0;
  vfi->singletonref_count = 0;
  vfi->singletonrefs = (ptr)0;
  vfi->tlc_count = 0;
  vfi->tlcs = (ptr)0;
  vfi->graph = make_vfasl_hash_table();

  /* First pass: determine sizes */

  for (s = 0; s < space_empty; s++) {
    vfasl_chunk *c;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = (ptr)0;
    c->length = 0;
    c->used = 0;
    c->swept = 0;
    c->next = (ptr)0;

    vfi->spaces[s].first = c;
    vfi->spaces[s].total_bytes = 0;
    vfi->spaces[s].ptr_bitmap = (ptr)0;
  }

  (void)vfasl_copy_all(vfi, v);

  for (s = 0; s < space_empty; s++) {
    vfasl_chunk *c, *next;
    for (c = vfi->spaces[s].first; c; c = next) {
      next = c->next;
      free(c->bytes);
      free(c);
    }
  }

  free_vfasl_hash_table(vfi->graph);

  /* Setup for second pass: allocate to contiguous bytes */

  size = 0;
  size += sizeof(uptr);
  size += sizeof(uptr) * (2 * space_empty + 1);
  size += sizeof(uptr); /* for sym_count */
  size += sizeof(uptr) + sizeof(uptr) * vfi->symref_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->closure_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->code_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->rtd_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->rtdref_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->singletonref_count;
  size += sizeof(uptr) + sizeof(uptr) * vfi->tlc_count;

  padding = ptr_align(size) - size;
  size += padding;

  data_size = 0;
  for (s = 0; s < space_empty; s++) {
    data_size += vfi->spaces[s].total_bytes;
  }

  bitmap_size = ptr_align((data_size + ((1 << log2_ptr_bits_per_byte)-1))  >> log2_ptr_bits_per_byte);
  size += data_size;
  pre_bitmap_size = size;
  size += ((space_empty + 1) * bitmap_size);

  bv = S_bytevector(size);
  memset(&BVIT(bv, 0), 0, size);

  p = &BVIT(bv, 0);

  /* Start header */
  
  p = ptr_add(p, sizeof(uptr));

  memcpy(p, &vfi->sym_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->symref_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->closure_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->code_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->rtd_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->rtdref_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->singletonref_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  memcpy(p, &vfi->tlc_count, sizeof(uptr));
  p = ptr_add(p, sizeof(uptr));
  
  for (s = 0; s < space_empty; s++) {
    memcpy(p, &vfi->spaces[s].total_bytes, sizeof(uptr));
    p = ptr_add(p, sizeof(uptr));
  }

  /* Skip writing bitmap flags and sizes unil we compact: */
  p = ptr_add(p, sizeof(uptr));
  bitmap_sizes_p = p;
  p = ptr_add(p, space_empty * sizeof(uptr));
  
  /* End header, start data */
  
  vfi->symrefs = p;
  p = ptr_add(p, sizeof(uptr) * vfi->symref_count);

  vfi->closures = p;
  p = ptr_add(p, sizeof(uptr) * vfi->closure_count);

  vfi->codes = p;
  p = ptr_add(p, sizeof(uptr) * vfi->code_count);

  vfi->base_rtd = S_G.base_rtd;
  vfi->rtds = p;
  p = ptr_add(p, sizeof(uptr) * vfi->rtd_count);
  
  vfi->rtdrefs = p;
  p = ptr_add(p, sizeof(uptr) * vfi->rtdref_count);

  vfi->singletonrefs = p;
  p = ptr_add(p, sizeof(uptr) * vfi->singletonref_count);

  vfi->tlcs = p;
  p = ptr_add(p, sizeof(uptr) * vfi->tlc_count);

  p = ptr_add(p, padding);

  vfi->sym_count = 0;
  vfi->symref_count = 0;
  vfi->closure_count = 0;
  vfi->code_count = 0;
  vfi->rtd_count = 0;
  vfi->rtdref_count = 0;
  vfi->singletonref_count = 0;
  vfi->tlc_count = 0;
  vfi->graph = make_vfasl_hash_table();

  vfi->base_addr = p;

  for (s = 0; s < space_empty; s++) {
    vfasl_chunk *c;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = p;
    c->length = vfi->spaces[s].total_bytes;
    c->used = 0;
    c->swept = 0;
    c->next = (ptr)0;
    vfi->spaces[s].first = c;

    p = ptr_add(p, vfi->spaces[s].total_bytes);
    vfi->spaces[s].total_bytes = 0;
  }

  p = ptr_add(p, bitmap_size); /* leave space for compacting */

  for (s = 0; s < space_empty; s++) {
    vfi->spaces[s].ptr_bitmap = p;
    p = ptr_add(p, bitmap_size);
  }

  /* Write data */

  v = vfasl_copy_all(vfi, v);
  
  *(uptr *)&BVIT(bv, 0) = ptr_diff(v, vfi->base_addr);

  /* Make all pointers are relative to the start of the bytevector */
  for (s = 0; s < space_empty; s++) {
    uptr *bm;
    ptr ptr_bitmap_end;
    bm = vfi->spaces[s].ptr_bitmap;
    ptr_bitmap_end = ptr_add(bm, bitmap_size);
    p = vfi->base_addr;
    for (; bm != ptr_bitmap_end; bm++) {
      if (*((uptr*)bm) == 0) {
        p = ptr_add(p, (sizeof(uptr) * 8) << log2_ptr_bytes);
      } else {
        uptr m = *bm;
        unsigned int j;
        for (j = 0; j < (sizeof(uptr) * 8); j++, m = m >> 1) {
          if (m & 1)
            (*(uptr *)p) -= (uptr)vfi->base_addr;
          p = ptr_add(p, sizeof(uptr));
        }
      }
    }
  }

  /* Compact bitmaps using a run-length encoding of zeros --- as long as that's shorter */
  {
    uptr *dest_bm = ptr_add(&BVIT(bv, 0), pre_bitmap_size);
    
    for (s = 0; s < space_empty; s++) {
      int compressed = 0;
      uptr sz;

      if (vfi->spaces[s].total_bytes == 0) {
        /* no bytes for this spacem, so there can't be any pointers into it */
        sz = 0;
      } else {
        uptr *bm;
        uptr i, j, zeros = 0;
        uptr bitmap_size_in_uptrs = bitmap_size >> log2_ptr_bytes;

        bm = vfi->spaces[s].ptr_bitmap;
        for (i = 0, j = 0; i < bitmap_size_in_uptrs; i++) {
          if (bm[i] == 0)
            zeros++;
          else {
            if (zeros) {
              if (j >= bitmap_size_in_uptrs) break;
              dest_bm[j++] = 0;
              if (j >= bitmap_size_in_uptrs) break;
              dest_bm[j++] = zeros;
            }
            zeros = 0;
            if (j >= bitmap_size_in_uptrs) break;
            dest_bm[j++] = bm[i];
          }
        }
        /* We can ignore trailing zeros */

        if (j < bitmap_size_in_uptrs) {
          /* compression worked */
          compressed = 1;
        } else {
          /* compaction didn't work */
          memcpy(dest_bm, bm, bitmap_size);
        }
        sz = j;
      }

      dest_bm += sz;
      sz <<= log2_ptr_bytes;
      ((uptr *)bitmap_sizes_p)[s] = sz;
      if (compressed)
        ((uptr *)bitmap_sizes_p)[-1] |= (1 << s);
    }

    /* Truncate bytevector to match end of bitmaps */
    {
      uptr sz = ptr_diff(dest_bm, &BVIT(bv, 0));
      BYTEVECTOR_TYPE(bv) = (sz << bytevector_length_offset) | type_bytevector;
    }
  }

  sort_offsets(vfi->symrefs, vfi->symref_count);
  sort_offsets(vfi->closures, vfi->closure_count);
  sort_offsets(vfi->codes, vfi->code_count);
  /* don't sort vfi->rtds, since parent-before-child matters */
  sort_offsets(vfi->rtdrefs, vfi->rtdref_count);
  sort_offsets(vfi->singletonrefs, vfi->singletonref_count);
  sort_offsets(vfi->tlcs, vfi->tlc_count);

  for (s = 0; s < space_empty; s++) {
    free(vfi->spaces[s].first);
  }

  free_vfasl_hash_table(vfi->graph);

  free(vfi);

  return bv;
}

static ptr vfasl_copy_all(vfasl_info *vfi, ptr v) {
  seginfo *si;
  ISPC s;
  int changed = 1;

  si = MaybeSegInfo(ptr_get_segment(v));
  
  v = copy(vfi, v, si);

  while (changed) {
    changed = 0;
    for (s = 0; s < space_empty; s++) {
      vfasl_chunk *c = vfi->spaces[s].first;
      while (c && (c->swept < c->used)) {
        ptr pp, pp_end;
        
        pp = ptr_add(c->bytes, c->swept);
        pp_end = ptr_add(c->bytes, c->used);
        c->swept = c->used;

        switch(s) {
        case space_pure:
        case space_impure:
        case space_weakpair:
        case space_ephemeron:
          while (pp < pp_end) {
            vfasl_relocate(vfi, pp);
            pp = ptr_add(pp, sizeof(ptr));
          }
          break;
        case space_symbol:
          while (pp < pp_end) {
            pp = ptr_add(pp, sweep(vfi, TYPE((ptr)pp, type_symbol)));
          }
          break;
        case space_pure_typed_object:
        case space_code:
        case space_impure_record:
          while (pp < pp_end) {
            pp = ptr_add(pp, sweep(vfi, TYPE((ptr)pp, type_typed_object)));
          }
          break;
        case space_data:
          break;
        default:
          S_error_abort("vfasl: shouldn't use this space");
          break;
        }

        c = c->next;
        changed = 1;
      }
    }
  }

  return v;
}

static void vfasl_register_pointer(vfasl_info *vfi, ptr p, ptr *pp) {
  if (vfi->spaces[0].ptr_bitmap) {
    /* Find right space's bitmap on the assumption that the spaces are
       ordered in their destination addresses, so we can search from
       the end */
    ISPC s;
    for (s = space_empty; --s; ) {
      if ((uptr)UNTYPE(p, TYPEBITS(p)) >= (uptr)vfi->spaces[s].first->bytes) {
        uptr delta = ptr_diff(pp, vfi->base_addr) >> log2_ptr_bytes;
        uptr i = delta >> (log2_ptr_bytes + 3);
        uptr bit = (((uptr)1) << (delta & ((1 << (log2_ptr_bytes + 3)) - 1)));
        vfi->spaces[s].ptr_bitmap[i] |= bit;
        return;
      }
    }
  }
}

static uptr ptr_base_diff(vfasl_info *vfi, ptr p)
{
  if ((uptr)vfi->base_addr > (uptr)UNTYPE(p, TYPEBITS(p)))
    S_error_abort("vfasl: pointer not in region");
    
  return ptr_diff(p, vfi->base_addr);
}

static void vfasl_register_symbol_reference(vfasl_info *vfi, ptr *pp, ptr p) {
  if (vfi->symrefs)
    vfi->symrefs[vfi->symref_count] = ptr_base_diff(vfi, pp);
  vfi->symref_count++;
  *pp = SYMVAL(p); /* replace symbol reference with index of symbol */
}

static void vfasl_register_closure(vfasl_info *vfi, ptr p) {
  if (vfi->closures)
    vfi->closures[vfi->closure_count] = ptr_base_diff(vfi, p);
  vfi->closure_count++;
}

static void vfasl_register_code(vfasl_info *vfi, ptr p) {
  if (vfi->codes)
    vfi->codes[vfi->code_count] = ptr_base_diff(vfi, p);
  vfi->code_count++;
}

static void vfasl_register_rtd(vfasl_info *vfi, ptr pp) {
  if (vfi->rtds)
    vfi->rtds[vfi->rtd_count] = ptr_base_diff(vfi, pp);
  vfi->rtd_count++;
}

static void vfasl_register_rtd_reference(vfasl_info *vfi, ptr pp) {
  if (vfi->rtdrefs)
    vfi->rtdrefs[vfi->rtdref_count] = ptr_base_diff(vfi, pp);
  vfi->rtdref_count++;
}

static void vfasl_register_tlc(vfasl_info *vfi, ptr p) {
  if (vfi->tlcs)
    vfi->tlcs[vfi->tlc_count] = ptr_base_diff(vfi, p);
  vfi->tlc_count++;
}

static void vfasl_register_singleton_reference(vfasl_info *vfi, ptr *pp, int which) {
  if (vfi->singletonrefs)
    vfi->singletonrefs[vfi->singletonref_count] = ptr_base_diff(vfi, pp);
  vfi->singletonref_count++;
  *pp = FIX(which);
}

static void vfasl_register_forward(vfasl_info *vfi, ptr pp, ptr p) {
  vfasl_hash_table_set(vfi->graph, pp, p);
}

static ptr vfasl_lookup_forward(vfasl_info *vfi, ptr p) {
  return vfasl_hash_table_ref(vfi->graph, p);
}

static ptr vfasl_find_room(vfasl_info *vfi, ISPC s, ITYPE t, iptr n) {
  ptr p;

  vfi->spaces[s].total_bytes += n;
  
  if (vfi->spaces[s].first->used + n > vfi->spaces[s].first->length) {
    vfasl_chunk *c;
    iptr newlen = n * 2;
    if (newlen < 4096)
      newlen = 4096;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = malloc(newlen);
    c->length = newlen;
    c->used = 0;
    c->swept = 0;
    
    c->next = vfi->spaces[s].first;
    vfi->spaces[s].first = c;
  }

  p = ptr_add(vfi->spaces[s].first->bytes, vfi->spaces[s].first->used);
  vfi->spaces[s].first->used += n;

  return TYPE(p, t);
}

#define FIND_ROOM(vfi, s, t, n, p) p = vfasl_find_room(vfi, s, t, n)

#define copy_ptrs(ty, p1, p2, n) {\
  ptr *Q1, *Q2, *Q1END;\
  Q1 = (ptr *)UNTYPE((p1),ty);\
  Q2 = (ptr *)UNTYPE((p2),ty);\
  Q1END = (ptr *)((uptr)Q1 + n);\
  while (Q1 != Q1END) *Q1++ = *Q2++;}

static ptr copy(vfasl_info *vfi, ptr pp, seginfo *si) {
    ptr p, tf; ITYPE t;

    if ((t = TYPEBITS(pp)) == type_typed_object) {
      tf = TYPEFIELD(pp);
      if (TYPEP(tf, mask_record, type_record)) {
          ptr rtd; iptr n; ISPC s;

          rtd = tf;

          n = size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));

          s = ((RECORDDESCPM(rtd) == FIX(-1))
               ? (RECORDDESCMPM(rtd) == FIX(0)
                  ? space_pure
                  : space_impure)
               : (RECORDDESCMPM(rtd) == FIX(0)
                  ? space_pure_typed_object
                  : space_impure_record));
          
          FIND_ROOM(vfi, s, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);

        /* pad if necessary */
          if (s == space_pure || s == space_impure) {
              iptr m = unaligned_size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
              if (m != n)
                  *((ptr *)((uptr)UNTYPE(p,type_typed_object) + m)) = FIX(0);
          }

          /* If the record is an rtd, then register it */
          if (tf == S_G.base_rtd) {
            if (pp == S_G.base_rtd)
              vfi->base_rtd = p;
            else if (vfi->base_rtd == S_G.base_rtd) {
              /* make sure base_rtd is first one registered */
              (void)vfasl_relocate_help(vfi, S_G.base_rtd);
            }
            /* need type and parent before child; FIXME: stack overflow possible */
            if (RECORDDESCPARENT(pp) != Sfalse) {
              (void)vfasl_relocate_help(vfi, RECORDDESCPARENT(pp));
            }
            vfasl_register_rtd(vfi, p);
          }
      } else if (TYPEP(tf, mask_vector, type_vector)) {
          iptr len, n;
          ISPC s;
          len = Svector_length(pp);
          n = size_vector(len);
        /* assumes vector lengths look like fixnums; if not, vectors will need their own space */
          s = (((uptr)tf & vector_immutable_flag)
               ? space_pure
               : space_impure);
          FIND_ROOM(vfi, s, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
        /* pad if necessary */
          if ((len & 1) == 0) INITVECTIT(p, len) = FIX(0);
      } else if (TYPEP(tf, mask_string, type_string)) {
          iptr n;
          n = size_string(Sstring_length(pp));
          FIND_ROOM(vfi, space_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_fxvector, type_fxvector)) {
          iptr n;
          n = size_fxvector(Sfxvector_length(pp));
          FIND_ROOM(vfi, space_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_bytevector, type_bytevector)) {
          iptr n;
          n = size_bytevector(Sbytevector_length(pp));
          FIND_ROOM(vfi, space_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if ((iptr)tf == type_tlc) {
          ptr keyval, next;

          /* FIXME */
          S_error("vfasl", "not ready for tlcs");

          FIND_ROOM(vfi, space_impure, type_typed_object, size_tlc, p);
          TLCTYPE(p) = type_tlc;
          INITTLCKEYVAL(p) = keyval = TLCKEYVAL(pp);
          INITTLCHT(p) = TLCHT(pp);
          INITTLCNEXT(p) = next = TLCNEXT(pp);

          vfasl_register_tlc(vfi, p);
      } else if (TYPEP(tf, mask_box, type_box)) {
          ISPC s;
          s = (((uptr)tf == type_immutable_box)
               ? space_pure
               : space_impure);
          FIND_ROOM(vfi, s, type_typed_object, size_box, p);
          BOXTYPE(p) = (iptr)tf;
          INITBOXREF(p) = Sunbox(pp);
      } else if ((iptr)tf == type_ratnum) {
          FIND_ROOM(vfi, space_data, type_typed_object, size_ratnum, p);
          RATTYPE(p) = type_ratnum;
          RATNUM(p) = RATNUM(pp);
          RATDEN(p) = RATDEN(pp);
      } else if ((iptr)tf == type_exactnum) {
          FIND_ROOM(vfi, space_data, type_typed_object, size_exactnum, p);
          EXACTNUM_TYPE(p) = type_exactnum;
          EXACTNUM_REAL_PART(p) = EXACTNUM_REAL_PART(pp);
          EXACTNUM_IMAG_PART(p) = EXACTNUM_IMAG_PART(pp);
      } else if ((iptr)tf == type_inexactnum) {
          FIND_ROOM(vfi, space_data, type_typed_object, size_inexactnum, p);
          INEXACTNUM_TYPE(p) = type_inexactnum;
          INEXACTNUM_REAL_PART(p) = INEXACTNUM_REAL_PART(pp);
          INEXACTNUM_IMAG_PART(p) = INEXACTNUM_IMAG_PART(pp);
      } else if (TYPEP(tf, mask_bignum, type_bignum)) {
          iptr n;
          n = size_bignum(BIGLEN(pp));
          FIND_ROOM(vfi, space_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_port, type_port)) {
          vfasl_fail(vfi, "port");
          return (ptr)0;
      } else if (TYPEP(tf, mask_code, type_code)) {
          iptr n;
          n = size_code(CODELEN(pp));
          FIND_ROOM(vfi, space_code, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
          if (CODERELOC(pp) == (ptr)0) {
            /* We only get here if we're vfasling code that belongs in
               the static generation. */
            ptr l; iptr ln;
            ln = size_reloc_table(0);
            FIND_ROOM(vfi, space_data, typemod, ln, l);
            RELOCSIZE(l) = 0;
            RELOCCODE(l) = p;
            CODERELOC(p) = l;
            vfasl_register_pointer(vfi, l, &CODERELOC(p));
          }
      } else if ((iptr)tf == type_rtd_counts) {
          FIND_ROOM(vfi, space_data, type_typed_object, size_rtd_counts, p);
          copy_ptrs(type_typed_object, p, pp, size_rtd_counts);
      } else if ((iptr)tf == type_thread) {
          vfasl_fail(vfi, "thread");
          return (ptr)0;
      } else {
          S_error_abort("vfasl: illegal type");
          return (ptr)0 /* not reached */;
      }
    } else if (t == type_pair) {
      if (si->space == space_ephemeron) {
        FIND_ROOM(vfi, space_ephemeron, type_pair, size_ephemeron, p);
        EPHEMERONNEXT(p) = Snil;
        EPHEMERONTRIGGERNEXT(p) = Snil;
      } else if (si->space == space_weakpair) {
        FIND_ROOM(vfi, space_weakpair, type_pair, size_pair, p);
      } else {
        FIND_ROOM(vfi, space_impure, type_pair, size_pair, p);
      }
      INITCAR(p) = Scar(pp);
      INITCDR(p) = Scdr(pp);
    } else if (t == type_closure) {
        ptr code;
        code = CLOSCODE(pp);
        if (CODETYPE(code) & (code_flag_continuation << code_flags_offset)) {
          vfasl_fail(vfi, "continuation");
          return (ptr)0;
        } else {
            iptr len, n;
            len = CLOSLEN(pp);
            n = size_closure(len);
            if (CODETYPE(code) & (code_flag_mutable_closure << code_flags_offset)) {
              /* Using `space_impure` is ok because the code slot of a mutable
                 closure is never mutated, so the code is never newer than the
                 closure. If it were, then because the code pointer looks like
                 a fixnum, an old-generation sweep wouldn't update it properly. */
              FIND_ROOM(vfi, space_impure, type_closure, n, p);
            } else {
              FIND_ROOM(vfi, space_pure, type_closure, n, p);
            }
            copy_ptrs(type_closure, p, pp, n);
         /* pad if necessary */
            if ((len & 1) == 0) CLOSIT(p, len) = FIX(0);

            /* To code-entry pointer looks like an immediate to
               sweep, so relocate the code directly, and also make it
               relative to the base address. */
            code = vfasl_relocate_help(vfi, code);
            code = (ptr)ptr_diff(code, vfi->base_addr);
            SETCLOSCODE(p,code);
            vfasl_register_closure(vfi, p);
        }
    } else if (t == type_symbol) {
        iptr pos = vfi->sym_count++;
        FIND_ROOM(vfi, space_symbol, type_symbol, size_symbol, p);
        INITSYMVAL(p) = FIX(pos);   /* stores symbol index for now; will get reset on load */
        INITSYMPVAL(p) = Snil;      /* will get reset on load */
        INITSYMPLIST(p) = Snil;
        INITSYMSPLIST(p) = Snil;
        INITSYMNAME(p) = SYMNAME(pp);
        INITSYMHASH(p) = Sfalse;
    } else if (t == type_flonum) {
        FIND_ROOM(vfi, space_data, type_flonum, size_flonum, p);
        FLODAT(p) = FLODAT(pp);
        return p;
    } else {
      S_error_abort("copy(gc): illegal type");
      return (ptr)0 /* not reached */;
    }

    vfasl_register_forward(vfi, pp, p);

    return p;
}

static ptr vfasl_relocate_help(vfasl_info *vfi, ptr pp) {
  ptr fpp;
  seginfo *si;
  
  si = MaybeSegInfo(ptr_get_segment(pp));
  if (!si)
    vfasl_fail(vfi, "unknown");
  
  fpp = vfasl_lookup_forward(vfi, pp);
  if (fpp)
    return fpp;
  else
    return copy(vfi, pp, si);
}

/* Use vfasl_relocate only on addresses that are in the vfasl target area */
static void vfasl_relocate(vfasl_info *vfi, ptr *ppp) {
  ptr pp = *ppp, tf;
  if (!IMMEDIATE(pp)) {
    int which_singleton;
    if ((which_singleton = detect_singleton(pp)))
      vfasl_register_singleton_reference(vfi, ppp, which_singleton);
    else {
      pp = vfasl_relocate_help(vfi, pp);
      *ppp = pp;
      if (!IMMEDIATE(pp)) {
        if (TYPEBITS(pp) == type_symbol)
          vfasl_register_symbol_reference(vfi, ppp, pp);
        else {
          if ((TYPEBITS(pp) == type_typed_object)
              && (((tf = TYPEFIELD(pp)) == vfi->base_rtd)
                  || (tf == S_G.base_rtd)))
            vfasl_register_rtd_reference(vfi, ppp);
          vfasl_register_pointer(vfi, pp, ppp);
        }
      }
    }
  }
}

static void sweep_ptrs(vfasl_info *vfi, ptr *pp, iptr n) {
  ptr *end = pp + n;

  while (pp != end) {
    vfasl_relocate(vfi, pp);
    pp += 1;
  }
}

static uptr sweep(vfasl_info *vfi, ptr p) {
  ptr tf; ITYPE t;

  if ((t = TYPEBITS(p)) == type_pair) {
    vfasl_relocate(vfi, &INITCAR(p));
    vfasl_relocate(vfi, &INITCDR(p));
    return size_pair;
  } else if (t == type_closure) {
    uptr len;

    len = CLOSLEN(p);
    sweep_ptrs(vfi, &CLOSIT(p, 0), len);

    return size_closure(len);
  } else if (t == type_symbol) {
    vfasl_relocate(vfi, &INITSYMNAME(p));
      /* other parts are replaced on load */
    return size_symbol;
  } else if (t == type_flonum) {
    /* nothing to sweep */;
    return size_flonum;
 /* typed objects */
  } else if (tf = TYPEFIELD(p), TYPEP(tf, mask_vector, type_vector)) {
    uptr len = Svector_length(p);
    sweep_ptrs(vfi, &INITVECTIT(p, 0), len);
    return size_vector(len);
  } else if (TYPEP(tf, mask_string, type_string)) {
    /* nothing to sweep */;
    return size_string(Sstring_length(p));
  } else if (TYPEP(tf, mask_bytevector, type_bytevector)) {
    /* nothing to sweep */;
    return size_bytevector(Sbytevector_length(p));
  } else if (TYPEP(tf, mask_fxvector, type_fxvector)) {
    /* nothing to sweep */;
    return size_fxvector(Sfxvector_length(p));
  } else if (TYPEP(tf, mask_record, type_record)) {
    return sweep_record(vfi, p);
  } else if (TYPEP(tf, mask_box, type_box)) {
    vfasl_relocate(vfi, &INITBOXREF(p));
    return size_box;
  } else if ((iptr)tf == type_ratnum) {
    vfasl_relocate(vfi, &RATNUM(p));
    vfasl_relocate(vfi, &RATDEN(p));
    return size_ratnum;
  } else if ((iptr)tf == type_tlc) {
    vfasl_relocate(vfi, &INITTLCKEYVAL(p));
    vfasl_relocate(vfi, &INITTLCHT(p));
    vfasl_relocate(vfi, &INITTLCNEXT(p));
    return size_tlc;
  } else if ((iptr)tf == type_exactnum) {
    vfasl_relocate(vfi, &EXACTNUM_REAL_PART(p));
    vfasl_relocate(vfi, &EXACTNUM_IMAG_PART(p));
    return size_exactnum;
  } else if ((iptr)tf == type_inexactnum) {
    /* nothing to sweep */;
    return size_inexactnum;
  } else if (TYPEP(tf, mask_bignum, type_bignum)) {
    /* nothing to sweep */;
    return size_bignum(BIGLEN(p));
  } else if (TYPEP(tf, mask_code, type_code)) {
    return sweep_code_object(vfi, p);
  } else {
    S_error_abort("vfasl_sweep: illegal type");
    return 0;
  }
}

static uptr sweep_record(vfasl_info *vfi, ptr x)
{
    ptr *pp; ptr num; ptr rtd;

    rtd = RECORDINSTTYPE(x);
    vfasl_relocate(vfi, &RECORDINSTTYPE(x));
    
    num = RECORDDESCPM(rtd);
    pp = &RECORDINSTIT(x,0);

    /* process cells for which bit in pm is set; quit when pm == 0. */
    if (Sfixnump(num)) {
      /* ignore bit for already forwarded rtd */
        uptr mask = (uptr)UNFIX(num) >> 1;
        if (mask == (uptr)-1 >> 1) {
          ptr *ppend = (ptr *)((uptr)pp + UNFIX(RECORDDESCSIZE(rtd))) - 1;
          while (pp < ppend) {
            vfasl_relocate(vfi, pp);
            pp += 1;
          }
        } else {
          while (mask != 0) {
            if (mask & 1) vfasl_relocate(vfi, pp);
            mask >>= 1;
            pp += 1;
          }
        }
    } else {
      iptr index; bigit mask; INT bits;

      /* bignum pointer mask */
      num = RECORDDESCPM(rtd);
      vfasl_relocate(vfi, &RECORDDESCPM(rtd));
      index = BIGLEN(num) - 1;
      /* ignore bit for already forwarded rtd */
      mask = BIGIT(num,index) >> 1;
      bits = bigit_bits - 1;
      for (;;) {
        do {
          if (mask & 1) vfasl_relocate(vfi, pp);
          mask >>= 1;
          pp += 1;
        } while (--bits > 0);
        if (index-- == 0) break;
        mask = BIGIT(num,index);
        bits = bigit_bits;
      }
    }

    return size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
}

#define VFASL_RELOC_TAG_BITS         3

#define VFASL_RELOC_C_ENTRY_TAG            1
#define VFASL_RELOC_LIBRARY_ENTRY_TAG      2
#define VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG 3
#define VFASL_RELOC_SYMBOL_TAG             4
#define VFASL_RELOC_SINGLETON_TAG          5

#define VFASL_RELOC_C_ENTRY(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_C_ENTRY_TAG)
#define VFASL_RELOC_LIBRARY_ENTRY(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_LIBRARY_ENTRY_TAG)
#define VFASL_RELOC_LIBRARY_ENTRY_CODE(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)
#define VFASL_RELOC_SYMBOL(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_SYMBOL_TAG)
#define VFASL_RELOC_SINGLETON(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_SINGLETON_TAG)

#define VFASL_RELOC_TAG(p) (UNFIX(p) & ((1 << VFASL_RELOC_TAG_BITS) - 1))
#define VFASL_RELOC_POS(p) (UNFIX(p) >> VFASL_RELOC_TAG_BITS)

static uptr sweep_code_object(vfasl_info *vfi, ptr co) {
    ptr t, oldco, oldt; iptr a, m, n;

    vfasl_relocate(vfi, &CODENAME(co));
    vfasl_relocate(vfi, &CODEARITYMASK(co));
    vfasl_relocate(vfi, &CODEINFO(co));
    vfasl_relocate(vfi, &CODEPINFOS(co));

    oldt = CODERELOC(co);

    n = size_reloc_table(RELOCSIZE(oldt));
    t = vfasl_find_room(vfi, space_data, typemod, n);
    copy_ptrs(typemod, t, oldt, n);

    m = RELOCSIZE(t);
    oldco = RELOCCODE(t);
    a = 0;
    n = 0;
    while (n < m) {
        uptr entry, item_off, code_off; ptr obj, pos;
        int which_singleton;
 
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);

        if ((which_singleton = detect_singleton(obj))) {
          obj = FIX(VFASL_RELOC_SINGLETON(which_singleton));
        } else if ((pos = vfasl_hash_table_ref(S_G.c_entries, obj))) {
          obj = FIX(VFASL_RELOC_C_ENTRY(pos));
        } else if ((pos = vfasl_hash_table_ref(S_G.library_entries, obj))) {
          obj = FIX(VFASL_RELOC_LIBRARY_ENTRY(pos));
        } else if ((pos = vfasl_hash_table_ref(S_G.library_entry_codes, obj))) {
          obj = FIX(VFASL_RELOC_LIBRARY_ENTRY_CODE(pos));
        } else if (Ssymbolp(obj)) {
          obj = vfasl_relocate_help(vfi, obj);
          obj = FIX(VFASL_RELOC_SYMBOL(UNFIX(SYMVAL(obj))));
        } else if (IMMEDIATE(obj)) {
          /* as-is */
          if (Sfixnump(obj))
            S_error("vfasl", "unexpected fixnum in relocation");
        } else {
          obj = vfasl_relocate_help(vfi, obj);
          obj = (ptr)ptr_diff(obj, vfi->base_addr);
        }

        S_set_code_obj("vfasl", RELOC_TYPE(entry) | reloc_force_abs, co, a, obj, item_off);
    }

    RELOCCODE(t) = co;
    CODERELOC(co) = t;

    vfasl_register_pointer(vfi, co, &RELOCCODE(t));
    vfasl_register_pointer(vfi, t, &CODERELOC(co));

    vfasl_register_code(vfi, co);

    return size_code(CODELEN(co));
}

static void relink_code(ptr co, ptr sym_base, ptr *spaces, uptr *space_offsets) {
    ptr t; iptr a, m, n;

    t = CODERELOC(co);

    m = RELOCSIZE(t);
    a = 0;
    n = 0;
    while (n < m) {
      uptr entry, item_off, code_off; ptr obj;
 
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry) | reloc_force_abs, co, a, item_off);

        if (IMMEDIATE(obj)) {
          if (Sfixnump(obj)) {
            int tag = VFASL_RELOC_TAG(obj);
            int pos = VFASL_RELOC_POS(obj);
            if (tag == VFASL_RELOC_SINGLETON_TAG)
              obj = lookup_singleton(pos);
            else if (tag == VFASL_RELOC_C_ENTRY_TAG)
              obj = S_lookup_c_entry(pos);
            else if ((tag == VFASL_RELOC_LIBRARY_ENTRY_TAG)
                     || (tag == VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)) {
              obj = S_lookup_library_entry(pos, 1);
              if (tag == VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)
                obj = CLOSCODE(obj);
            } else if (tag == VFASL_RELOC_SYMBOL_TAG) {
              ptr val;
              obj = TYPE(ptr_add(sym_base, pos * size_symbol), type_symbol);
              if ((val = SYMVAL(obj)) != sunbound)
                obj = val;
            } else {
              S_error_abort("vfasl: bad relocation tag");
            }
          } else {
            /* some other immediate, such as black-hole; leave as-is */
          }
        } else {
          /* most common case is reference to other code */
          uptr offset = (uptr)UNTYPE(obj, TYPEBITS(obj));
          ptr base;
          if (offset >= space_offsets[space_code] && offset < space_offsets[space_code+1]) {
            base = ptr_subtract(spaces[space_code], space_offsets[space_code]);
            obj = ptr_add(base, (uptr)obj);
          } else {
            /* have to search */
            ISPC s;
            base = (ptr)0;
            for (s = 0; s < space_empty; s++) {
              if (offset >= space_offsets[s] && offset < space_offsets[s+1]) {
                base = ptr_subtract(spaces[s], space_offsets[s]);
                break;
              }
            }
            obj = ptr_add(base, (uptr)obj);
            if ((TYPEBITS(obj) == type_typed_object)
                && (TYPEFIELD(obj) == S_G.base_rtd)) {
              /* Similar to symbols: potentially replace with interned */
              ptr uid = RECORDDESCUID(obj);
              if (!Ssymbolp(uid)) {
                /* "uid" is actually the interned rtd to use instead */
                obj = uid;
              }
            }
          }
        }

        S_set_code_obj("vfasl", RELOC_TYPE(entry), co, a, obj, item_off);
    }
}

/*************************************************************/

static void fasl_init_entry_tables()
{
  tc_mutex_acquire()

  if (!S_G.c_entries) {
    iptr i;
    
    S_G.c_entries = make_vfasl_hash_table();
    S_G.library_entries = make_vfasl_hash_table();
    S_G.library_entry_codes = make_vfasl_hash_table();

    for (i = Svector_length(S_G.c_entry_vector); i--; ) {
      ptr entry = Svector_ref(S_G.c_entry_vector, i);
      vfasl_hash_table_set(S_G.c_entries, entry, (ptr)i);
    }

    for (i = Svector_length(S_G.library_entry_vector); i--; ) {
      ptr entry = Svector_ref(S_G.library_entry_vector, i);
      if (entry != Sfalse) {
        vfasl_hash_table_set(S_G.library_entries, entry, (ptr)i);
        vfasl_hash_table_set(S_G.library_entry_codes, CLOSCODE(entry), (ptr)i);
      }
    }
  }

  tc_mutex_release()
}

/*************************************************************/

static int detect_singleton(ptr p) {
  if (p == S_G.null_string)
    return 1;
  else if (p == S_G.null_vector)
    return 2;
  else if (p == S_G.null_fxvector)
    return 3;
  else if (p == S_G.null_bytevector)
    return 4;
  else
    return 0;
}

static ptr lookup_singleton(int which) {
  switch (which) {
  case 1:
    return S_G.null_string;
  case 2:
    return S_G.null_vector;
  case 3:
    return S_G.null_fxvector;
  case 4:
    return S_G.null_bytevector;
  default:
    S_error("vfasl", "bad singleton index");
    return (ptr)0;
  }
}
  
/*************************************************************/

#define HASH_CODE(p) ((uptr)(p) >> log2_ptr_bytes)
#define HASH_CODE2(p) (((uptr)(p) >> (log2_ptr_bytes + log2_ptr_bytes)) | 1)

static vfasl_hash_table *make_vfasl_hash_table() {
  vfasl_hash_table *ht;

  ht = malloc(sizeof(vfasl_hash_table));
  
  ht->count = 0;
  ht->size = 16;
  ht->entries = calloc(sizeof(hash_entry), ht->size);

  return ht;
}

static void free_vfasl_hash_table(vfasl_hash_table *ht) {
  free(ht->entries);
  free(ht);
}

static void vfasl_hash_table_set(vfasl_hash_table *ht, ptr key, ptr value) {
  uptr hc = HASH_CODE(key);
  uptr hc2 = HASH_CODE2(key);
  uptr size = ht->size;
    
  if (ht->count > ht->size >> 1) {
    /* rehash */
    uptr i;
    hash_entry *old_entries = ht->entries;
    
    ht->count = 0;
    ht->size *= 2;
    ht->entries = calloc(sizeof(hash_entry), ht->size);
    
    for (i = 0; i < size; i++) {
      if (old_entries[i].key)
        vfasl_hash_table_set(ht, old_entries[i].key, old_entries[i].value);
    }
    
    free(old_entries);
    size = ht->size;
  }

  hc = hc & (size - 1);
  while (ht->entries[hc].key) {
    hc = (hc + hc2) & (size - 1);
  }

  ht->entries[hc].key = key;
  ht->entries[hc].value = value;
  ht->count++;
}

static ptr vfasl_hash_table_ref(vfasl_hash_table *ht, ptr key) {
  uptr hc = HASH_CODE(key);
  uptr hc2 = HASH_CODE2(key);
  uptr size = ht->size;
  ptr old_key;
  
  hc = hc & (size - 1);
  while ((old_key = ht->entries[hc].key) != key) {
    if (!old_key)
      return (ptr)0;
    hc = (hc + hc2) & (size - 1);
  }

  return ht->entries[hc].value;
}

/*************************************************************/

static void sort_offsets(uptr *p, uptr len)
{
  while (1) {
    if (len > 1) {
      uptr i, pivot = 0;

      {
        uptr mid = len >> 2;
        uptr tmp = p[mid];
        p[mid] = p[0];
        p[0] = tmp;
      }
      
      for (i = 1; i < len; i++) {
        if (p[i] < p[pivot]) {
          uptr tmp = p[pivot];
          p[pivot] = p[i];
          pivot++;
          p[i] = p[pivot];
          p[pivot] = tmp;
        }
      }

      if (pivot > (len >> 1)) {
        sort_offsets(p+pivot+1, len-pivot-1);
        len = pivot;
      } else {
        sort_offsets(p, pivot);
        p = p+pivot+1;
        len = len-pivot-1;
      }
    } else
      return;
  }
}

;; This file defines the traversal of objects for the GC and similar
;; purposes. The description supports the generatation of multiple C
;; functions, each specialized to a particular traversal mode, while
;; sharing the overall traversal implementation.

;; record mapping from selectors to C macros:
(let-values ([(op get) (open-bytevector-output-port (native-transcoder))])
  (mkequates.h op))

(disable-unbound-warning
 trace-base-types
 trace-object-types
 trace-macros
 mkgc-ocd.inc
 mkgc-oce.inc
 )

(define trace-base-types '())
(define trace-object-types '())
(define trace-macros (make-eq-hashtable))

(define-syntax define-trace-root
  (syntax-rules (case-type typed-object case-typedfield)
    [(_ (case-type
         [type type-rhs ...]
         ...
         [typed-object
          (case-typefield
           [object-type object-type-rhs ...]
           ...)]))
     (begin
       (set! trace-base-types '((type type-rhs ...) ...))
       (set! trace-object-types '((object-type object-type-rhs ...) ...)))]))

(define-syntax define-trace-macro
  (syntax-rules ()
    [(_ (id arg ...) body ...)
     (eq-hashtable-set! trace-macros 'id '((arg ...) body ...))]))

;; Modes:
;;   - copy
;;   - sweep
;;   - self-test : trace immediate pointers only for self references
;;   - size      : does not recur
;;   - measure

;; Primitive actions/declarations:
;;  - (space <space>) : target for copy; constraint for other modes
;;  - (size <size>) : size for copy
;;  - (trace <field>) : relocate for sweep, copy for copy, recur otherwise
;;  - (trace-early <field>) : relocate for sweep orr copy, recur otherwise
;;  - (trace-now <field>) : direct recur
;;  - (trace-early-rtd <field>) : for record types, avoid recur on #!base-trd
;;  - (trace-return <field>) : for return address
;;  - (trace-ptrs <field> count) : trace an array of pointerrs
;;  - (copy <field>) : copy for copy, ignore otherwise
;;  - (copy-bytes <field> count) ; copy an array of bytes
;   - (copy-flonum <field>) ; copy flonum and forward
;   - (copy-flonum* <field>) ; copy potentially forwaded flonum
;;  - (count <counter> [<size>]) : uses `size` declaration unless <size>
;;  - (pad <condition> <lhs>)
;;  - (skip-forwarding)

(define-trace-root
  (case-type
   
   [pair
    (case-space
     [space-ephemeron
      (space space-ephemeron)
      (size size-ephemeron)
      (copy pair-car)
      (copy pair-cdr)
      (add-ephemeron-to-pending)
      (count countof-ephemeron)]
     [space-weakpair
      (space space-weakpair)
      (try-double-pair copy pair-car
                       trace pair-cdr
                       countof-weakpair)]
     [else
      (space space-impure) 
      (try-double-pair trace pair-car
                       trace pair-cdr
                       countof-pair)])]

   [closure
    (define code : ptr (CLOSCODE _))
    (trace-early (just code))
    (cond
      [(& (code-type code) (<< code-flag-continuation code-flags-offset))
       ;; continuation
       (space space-continuation)
       (size size-continuation)
       (case-mode
        [self-test]
        [else
         (copy-clos-code code)
         (copy-stack-length continuation-stack-length continuation-stack-clength)
         (copy continuation-stack-clength)
         (trace-nonself continuation-winders)
         (trace-nonself continuation-attachments)
         (cond
           [(== (continuation-stack-length _) scaled-shot-1-shot-flag)]
           [else
            (case-mode
             [sweep
              (when (OLDSPACE (continuation-stack _))
                (set! (continuation-stack _)
                      (copy_stack (continuation-stack _)
                                  (& (continuation-stack-length _))
                                  (continuation-stack-clength _))))]
             [else])
            (trace continuation-link)
            (trace-return continuation-return-address (continuation-return-address _))
            (case-mode
             [copy (copy continuation-stack)]
             [else
              (define stack : uptr (cast uptr (continuation-stack _)))
              (trace-stack stack
                           (+ stack (continuation-stack-clength _))
                           (cast uptr (continuation-return-address _)))])])
         (count countof-continuation)])]

      [else
       ;; closure (not a continuation)
       (space
        (case-backreferences
         [keep-backreferences
          space-closure]
         [no-backreferences
          (cond
            [(& (code-type code) (<< code-flag-mutable-closure code-flags-offset))
             space-impure]
            [else
             space-pure])]))
       (define len : uptr (code-closure-length code))
       (size (size_closure len))
       (copy-clos-code code)
       (trace-ptrs closure-data len)
       (pad (when (== (& len 1) 0)
              (set! (CLOSIT _copy_ len) (FIX 0))))
       (count countof-closure)])]
   
   [symbol
    (space space-symbol)
    (size size-symbol)
    (trace/define symbol-value val)
    (trace-symcode symbol-pvalue val)
    (trace-nonself symbol-plist)
    (trace-nonself symbol-name)
    (trace-nonself symbol-splist)
    (trace-nonself symbol-hash)
    (count countof-symbol)]
   
   [flonum
    (space space-data)
    (size size-flonum)
    (copy-flonum flonum-data)
    (count countof-flonum)
    (skip-forwarding)]
   
   [typed-object
    (case-typefield

     [record
      (trace-early-rtd record-type)
      ;; If the rtd is the only pointer and is immutable, put the record
      ;; into space-data. If the record contains only pointers, put it
      ;; into space-pure or space-impure. Otherwise, put it into
      ;; space-pure-typed-object or space-impure-record. We could put all
      ;; records into space-{pure,impure}-record or even into
      ;; space-impure-record, but by picking the target space more
      ;; carefully, we may reduce fragmentation and sweeping cost.
      (define rtd : ptr (record-type _))
      (space
       (cond
         [(&& (== (record-type-pm rtd) (FIX 1))
              (== (record-type-mpm rtd) (FIX 0)))
          ;; No pointers except for type
          space-data]
         [(== (record-type-pm rtd) (FIX -1))
          ;; All pointers
          (case-backreferences
           [keep-backreferences
            (cond
              [(== (record-type-mpm rtd) (FIX 0))
               ;; All immutable
               space-pure-typed-object]
              [else
               space-impure-record])]
           [no-backreferences
            (cond
              [(== (record-type-mpm rtd) (FIX 0))
               ;; All immutable
               space-pure]
              [else
               space-impure])])]
         [else
          ;; Mixture of pointers and non-pointers
          (cond
            [(== (record-type-mpm rtd) (FIX 0))
             ;; All immutable
             space-pure-typed-object]
            [else
             space-impure-record])]))
      (define len : uptr (UNFIX (record-type-size rtd)))
      (size (size_record_inst len))
      (trace-record rtd len)
      (pad (when (\|\| (== p_spc space_pure) (== p_spc space_impure))
             (let* ([ua_size : uptr (unaligned_size_record_inst len)])
               (when (!= p_sz ua_size)
                 (set! (* (cast ptr* (+ (cast uptr (UNTYPE _copy_ type_typed_object)) ua_size)))
                       (FIX 0))))))
      (count-record rtd)]

     [vector
      ;; Assumes vector lengths look like fixnums;
      ;; if not, vectors will need their own space
      (space
       (cond
         [(& (cast uptr tf) vector_immutable_flag)
          (case-backreferences
           [keep-backreferences space-pure-typed-object]
           [no-backreferences space-pure])]
         [else
          (case-backreferences
           [keep-backreferences space-impure-typed-object]
           [no-backreferences space-impure])]))
      (define len : uptr (Svector_length _))
      (size (size_vector len))
      (copy-type vector-type)
      (trace-ptrs vector-data len)
      (pad (when (== (& len 1) 0)
             (set! (INITVECTIT _copy_ len) (FIX 0))))
      (count countof-vector)]

     [stencil-vector
      ;; Assumes stencil-vector masks look like fixnums;
      ;; if not, stencil vectors will need their own space
      (space
       (case-backreferences
        [keep-backreferences space-impure-typed-object]
        [no-backreferences space-impure]))
      (define len : uptr (Sstencil_vector_length _))
      (size (size_stencil_vector len))
      (copy-type stencil-vector-type)
      (trace-ptrs stencil-vector-data len)
      (pad (when (== (& len 1) 0)
             (set! (INITSTENVECTIT _copy_ len) (FIX 0))))
      (count countof-stencil-vector)]

     [string
      (space space-data)
      (define-for-size sz : uptr (size_string (Sstring_length _)))
      (size (just sz))
      (copy-bytes string-type sz)
      (count countof-string)]

     [fxvector
      (space space-data)
      (define-for-size sz : uptr (size_fxvector (Sfxvector_length _)))
      (size (just sz))
      (copy-bytes fxvector-type sz)
      (count countof-fxvector)]

     [bytevector
      (space space-data)
      (define-for-size sz : uptr (size_bytevector (Sbytevector_length _)))
      (size (just sz))
      (copy-bytes bytevector-type sz)
      (count countof-bytevector)]

     [tlc
      (space
       (case-backreferences
        [keep-backreferences space-impure-typed-object]
        [no-backreferences space-impure]))
      (size size-tlc)
      (copy-type tlc-type)
      (trace-nonself tlc-ht)
      (trace-tlc tlc-next tlc-keyval)
      (count countof-tlc)]

     [box
      (space
       (cond
         [(== (box-type _) type-immutable-box)
          (case-backreferences
           [keep-backreferences space-pure-typed-object]
           [no-backreferences space-pure])]
         [else
          (case-backreferences
           [keep-backreferences space-impure-typed-object]
           [no-backreferences space-impure])]))
      (size size-box)
      (copy-type box-type)
      (trace box-ref)
      (count countof-box)]

     [ratnum
      (space space-data)
      (size size-ratnum)
      (copy-type ratnum-type)
      (trace-now ratnum-numerator)
      (trace-now ratnum-denominator)
      (count countof-ratnum)]

     [exactnum
      (space space-data)
      (size size-exactnum)
      (copy-type exactnum-type)
      (trace-now exactnum-real)
      (trace-now exactnum-imag)
      (count countof-exactnum)]

     [inexactnum
      (space space-data)
      (size size-inexactnum)
      (copy-type inexactnum-type)
      (copy-flonum* inexactnum-real)
      (copy-flonum* inexactnum-imag)
      (count countof-inexactnum)]

     [bignum
      (space space-data)
      (define-for-size sz : uptr (size_bignum (BIGLEN _)))
      (size (just sz))
      (copy-bytes bignum-type sz)
      (count countof-bignum)]

     [port
      (space space-port)
      (size size-port)
      (copy-type port-type)
      (trace-nonself port-handler)
      (copy port-ocount)
      (copy port-icount)
      (trace-buffer PORT_FLAG_OUTPUT port-obuffer port-olast)
      (trace-buffer PORT_FLAG_INPUT port-ibuffer port-ilast)
      (trace port-info)
      (trace-nonself port-name)
      (count countof-port)]

     [code
      (space space-code)
      (define-for-size len : uptr (code-length _)) ; in bytes
      (size (size_code len))
      (copy-type code-type)
      (copy code-length)
      (copy code-reloc)
      (trace-nonself code-name)
      (trace-nonself code-arity-mask)
      (copy code-closure-length)
      (trace-nonself code-info)
      (trace-nonself code-pinfo*)
      (trace-code len)
      (count countof-code)]

     [thread
      (space space-pure-typed-object)
      (size size-thread)
      (case-mode
       [self-test]
       [else
        (copy-type thread-type)
        (trace-tc thread-tc)
        (count countof-thread)])]

     [rtd-counts
      (space space-data)
      (size size-rtd-counts)
      (copy-bytes rtd-counts-type size_rtd_counts)
      (count countof-rtd-counts)]

     [phantom
      (space space-data)
      (size size-phantom)
      (copy-type phantom-type)
      (copy phantom-length)
      (case-mode
       [copy (set! (array-ref S_G.phantom_sizes tg)
                   +=
                   (phantom-length _))]
       [else])])]))

(define-trace-macro (trace-nonself field)
  (case-mode
   [self-test]
   [else
    (trace field)]))

(define-trace-macro (define-for-size id : ty rhs)
  (case-mode
   [(copy size measure)
    (define id : ty rhs)]
   [else]))

(define-trace-macro (try-double-pair do-car pair-car
                                     do-cdr pair-cdr
                                     count-pair)
  (case-mode
   [copy
    ;; Try to copy two pairs at a time
    (define cdr_p : ptr (Scdr _))
    (define qsi : seginfo* NULL)
    (cond
      [(&& (!= cdr_p _)
           (&& (== (TYPEBITS cdr_p) type_pair)
               (&& (!= (set! qsi (MaybeSegInfo (ptr_get_segment cdr_p))) NULL)
                   (&& (== (-> qsi space) (-> si space))
                       (&& (!= (FWDMARKER cdr_p) forward_marker)
                           (! (locked cdr_p)))))))
       (check_triggers qsi)
       (size size-pair 2)
       (define new_cdr_p : ptr (cast ptr (+ (cast uptr _copy_) size_pair)))
       (set! (pair-car _copy_) (pair-car _))
       (set! (pair-cdr _copy_) new_cdr_p)
       (set! (pair-car new_cdr_p) (pair-car cdr_p))
       (set! (pair-cdr new_cdr_p) (pair-cdr cdr_p))
       (set! (FWDMARKER cdr_p) forward_marker)
       (set! (FWDADDRESS cdr_p) new_cdr_p)
       (case-backreferences
        [keep-backreferences (ADD_BACKREFERENCE_FROM new_cdr_p new_p)]
        [no-backreferences])
       (count count-pair size-pair 2)]
      [else
       (size size-pair) 
       (do-car pair-car)
       (do-cdr pair-cdr)
       (count count-pair)])]
   [else
    (size size-pair) 
    (do-car pair-car)
    (do-cdr pair-cdr)
    (count count-pair)]))

(define-trace-macro (copy-clos-code code)
  (case-mode
   [copy
    (SETCLOSCODE _copy_ code)]
   [sweep
    (SETCLOSCODE _copy_ code)]
   [else]))

(define-trace-macro (copy-stack-length continuation-stack-length continuation-stack-clength)
  (case-mode
   [copy
    ;; don't promote general one-shots, but promote opportunistic one-shots
    (cond
      [(== (continuation-stack-length _) opportunistic-1-shot-flag)
       (set! (continuation-stack-length _copy_) (continuation-stack-clength _))
       ;; ma`<y need to recur at end to promote link:
       (set! conts_to_promote (S_cons_in space_new 0 new_p conts_to_promote))]
      [else
       (copy continuation-stack-length)])]
   [else
    (copy continuation-stack-length)]))

(define-trace-macro (trace/define ref val)
  
  (case-mode
   [(copy measure)
    (trace ref)]
   [sweep
    (define val : ptr (ref _))
    (trace (just val))  
    (set! (ref _) val)]
   [else]))

(define-trace-macro (trace-symcode symbol-pvalue val)
  (case-mode
   [sweep
    (define code : ptr (cond
                         [(Sprocedurep val) (CLOSCODE val)]
                         [else (SYMCODE _)]))
    (trace (just code))
    (INITSYMCODE _ code)]
   [measure]
   [else
    (copy symbol-pvalue)]))

(define-trace-macro (trace-tlc tlc-next tlc-keyval)
  (case-mode
   [copy
    (define next : ptr (tlc-next _))
    (define keyval : ptr (tlc-keyval _))
    (set! (tlc-next _copy_) next)
    (set! (tlc-keyval _copy_) keyval)
    ;; If next isn't false and keyval is old, add tlc to a list of tlcs
    ;; to process later. Determining if keyval is old is a (conservative)
    ;; approximation to determining if key is old. We can't easily
    ;; determine if key is old, since keyval might or might not have been
    ;; swept already. NB: assuming keyvals are always pairs.
    (when (&& (!= next Sfalse) (& (SPACE keyval) space_old))
      (set! tlcs_to_rehash (S_cons_in space_new 0 _copy_ tlcs_to_rehash)))]
   [else
    (trace-nonself tlc-keyval)
    (trace-nonself tlc-next)]))

(define-trace-macro (trace-record trd len)
  (case-mode
   [copy
    (copy-bytes record-data len)]
   [else
    ;; record-type descriptor was forwarded already
    (let* ([num : ptr (RECORDDESCPM rtd)]
           [pp : ptr* (& (RECORDINSTIT _ 0))])
      ;; Process cells for which bit in pm is set, and quit when pm == 0
      (cond
        [(Sfixnump num)
         ;; Ignore bit for already forwarded rtd
         (let* ([mask : uptr (>> (cast uptr (UNFIX num)) 1)])
           (cond
             [(== mask (>> (cast uptr -1) 1))
              (let* ([ppend : ptr* (- (cast ptr* (+ (cast uptr pp) len)) 1)])
                (while
                 :? (< pp ppend)
                 (trace (* pp))
                 (set! pp += 1)))]
             [else
              (while
               :? (!= mask 0)
               (when (& mask 1)
                 (trace (* pp)))
               (set! mask >>= 1)
               (set! pp += 1))]))]
        [else
         ;; Bignum pointer mask may have been forwarded
         (trace (RECORDDESCPM rtd))
         (set! num (RECORDDESCPM rtd))
         (let* ([index : iptr (- (BIGLEN num) 1)]
                ;; Ignore bit for already forwarded rtd
                [mask : bigit (>> (BIGIT num index) 1)]
                [bits : INT (- bigit_bits 1)])
           (while
            :? 1
            (do-while
             (when (& mask 1)
               (trace (* pp)))
             (set! mask >>= 1)
             (set! pp += 1)
             (set! bits -= 1)
             ;; while:
             :? (> bits 0))
            (when (== index 0) break)
            (set! index -= 1)
            (set! mask (BIGIT num index))
            (set! bits bigit_bits)))]))]))

(define-trace-macro (trace-buffer flag port-buffer port-last)
  (case-mode
   [(copy measure)
    (copy port-last)
    (copy port-buffer)]
   [sweep
    (when (& (cast uptr tf) flag)
      (define n : iptr (- (cast iptr (port-last _))
                          (cast iptr (port-buffer _))))
      (trace port-buffer)
      (set! (port-last _) (cast ptr (+ (cast iptr (port-buffer _)) n))))]
   [else
    (trace-nonself port-buffer)]))

(define-trace-macro (trace-tc offset)
  (case-mode
   [copy
    (copy offset)]
   [else
    (define tc : ptr (cast ptr (offset _)))
    (when (!= tc (cast ptr 0))
      (case-mode
       [sweep
        (let* ([old_stack : ptr (tc-scheme-stack tc)])
          (when (OLDSPACE old_stack)
            (let* ([clength : iptr (- (cast uptr (SFP tc)) (cast uptr old_stack))])
              ;; Include SFP[0], which contains the return address
              (set! (tc-scheme-stack tc) (copy_stack old_stack
                                                     (& (tc-scheme-stack-size tc))
                                                     (+ clength (sizeof ptr))))
              (set! (SFP tc) (cast ptr (+ (cast uptr (tc-scheme-stack tc)) clength)))
              (set! (ESP tc) (cast ptr (- (+ (cast uptr (tc-scheme-stack tc))
                                             (tc-scheme-stack-size tc))
                                          stack_slop))))))]
       [else])
      (set! (tc-stack-cache tc) Snil)
      (trace (tc-cchain tc))
      (trace (tc-stack-link tc))
      (trace (tc-winders tc))
      (trace (tc-attachments tc))
      (case-mode
       [sweep
        (set! (tc-cached-frame tc) Sfalse)]
       [else])
      (trace-return NO-COPY (FRAME tc 0))
      (trace-stack (cast uptr (tc-scheme-stack tc))
                   (cast uptr (SFP tc))
                   (cast uptr (FRAME tc 0)))
      (trace (tc-U tc))
      (trace (tc-V tc))
      (trace (tc-W tc))
      (trace (tc-X tc))
      (trace (tc-Y tc))
      (trace (tc-threadno tc))
      (trace (tc-current-input tc))
      (trace (tc-current-output tc))
      (trace (tc-current-error tc))
      (trace (tc-sfd tc))
      (trace (tc-current-mso tc))
      (trace (tc-target-machine tc))
      (trace (tc-fxlength-bv tc))
      (trace (tc-fxfirst-bit-set-bv tc))
      (trace (tc-null-immutable-vector tc))
      (trace (tc-null-immutable-fxvector tc))
      (trace (tc-null-immutable-bytevector tc))
      (trace (tc-null-immutable-string tc))
      (trace (tc-compile-profile tc))
      (trace (tc-subset-mode tc))
      (trace (tc-default-record-equal-procedure tc))
      (trace (tc-default-record-hash-procedure tc))
      (trace (tc-compress-format tc))
      (trace (tc-compress-level tc))
      (trace (tc-parameters tc))
      (let* ([i : INT 0])
        (while
         :? (< i virtual_register_count)
         (trace (VIRTREG tc i))
         (set! i += 1))))]))

(define-trace-macro (trace-stack base-expr fp-expr ret-expr)
  (define base : uptr base-expr)
  (define fp : uptr fp-expr)
  (define ret : uptr ret-expr)

  (while
   :? (!= fp base)
   (when (< fp base)
     (S_error_abort "sweep_stack(gc): malformed stack"))
   (set! fp (- fp (ENTRYFRAMESIZE ret)))
   (let* ([pp : ptr* (cast ptr* fp)]
          [oldret : iptr ret])
     (set! ret (cast iptr (* pp)))
     (trace-return NO-COPY (* pp))
     (let* ([num : ptr (ENTRYLIVEMASK oldret)])
       (cond
         [(Sfixnump num)
          (let* ([mask : uptr (UNFIX num)])
            (while
             :? (!= mask 0)
             (set! pp += 1)
             (when (& mask #x0001)
               (trace (* pp)))
             (set! mask >>= 1)))]
         [else
          (trace (* (ENTRYNONCOMPACTLIVEMASKADDR oldret)))

          (let* ([num : ptr (ENTRYLIVEMASK oldret)]
                 [index : iptr (BIGLEN num)])
            (while
             :? (!= index 0)
             (set! index -= 1)
             (let* ([bits : INT bigit_bits]
                    [mask : bigit (BIGIT num index)])
               (while
                :? (> bits 0)
                (set! bits -= 1)
                (set! pp += 1)
                (when (& mask 1) (trace (* pp)))
                (set! mask >>= 1)))))])))))

(define-trace-macro (trace-return copy-field field)
  (case-mode
   [copy
    (copy copy-field)]
   [else
    (define xcp : ptr field)
    (case-mode
     [sweep
      (define x_si : seginfo* (SegInfo (ptr_get_segment xcp)))
      (when (& (-> x_si space) space_old)
        (trace-return-code field xcp x_si))]
     [else
      (trace-return-code field xcp no_x_si)])]))

(define-trace-macro (trace-return-code field xcp x_si)
  (define co : iptr (+ (ENTRYOFFSET xcp) (- (cast uptr xcp) (cast uptr (ENTRYOFFSETADDR xcp)))))
  ;; In the call to copy below, assuming SPACE(c_p) == SPACE(xcp) since
  ;; c_p and XCP point to/into the same object
  (define c_p : ptr (cast ptr (- (cast uptr xcp) co)))
  (case-mode
   [sweep
    (cond
      [(== (FWDMARKER c_p) forward_marker)
       (set! c_p (FWDADDRESS c_p))]
      [else
       (set! c_p (copy c_p x_si))])
    (set! field (cast ptr (+ (cast uptr c_p) co)))]
   [else
    (trace (just c_p))]))

(define-trace-macro (trace-code len)
  (case-mode
   [copy
    (copy-bytes code-data len)]
   [else
    (define t : ptr (CODERELOC _))
    (case-mode
     [sweep
      (define m : iptr (RELOCSIZE t))
      (define oldco : ptr (RELOCCODE t))]
     [else
      (define m : iptr (cond
                         [t (RELOCSIZE t)]
                         [else 0]))
      (define oldco : ptr (cond
                            [t (RELOCCODE t)]
                            [else 0]))])
    (define a : iptr 0)
    (define n : iptr 0)
    (while
     :? (< n m)
     (let* ([entry : uptr (RELOCIT t n)]
            [item_off : uptr 0]
            [code_off : uptr 0])
       (set! n (+ n 1))
       (cond
         [(RELOC_EXTENDED_FORMAT entry)
          (set! item_off (RELOCIT t n))
          (set! n (+ n 1))
          (set! code_off (RELOCIT t n))
          (set! n (+ n 1))]
         [else
          (set! item_off (RELOC_ITEM_OFFSET entry))
          (set! code_off (RELOC_CODE_OFFSET entry))])
       (set! a (+ a code_off))
       (let* ([obj : ptr (S_get_code_obj (RELOC_TYPE entry) oldco a item_off)])
         (trace (just obj))
         (case-mode
          [sweep
           (S_set_code_obj "gc" (RELOC_TYPE entry) _ a obj item_off)]
          [else]))))

    (case-mode
     [sweep
      (cond
        [(&& (== target_generation static_generation)
             (&& (! S_G.retain_static_relocation)
                 (== 0 (& (CODETYPE _) (<< code_flag_template code_flags_offset)))))
         (set! (CODERELOC _) (cast ptr 0))]
        [else
         ;; Don't copy non-oldspace relocation tables, since we may be
         ;; sweeping a locked code object that is older than target_generation.
         ;; Doing so would be a waste of work anyway.
         (when (OLDSPACE t)
           (let* ([oldt : ptr t])
             (set! n (size_reloc_table (RELOCSIZE oldt)))
             (count countof-relocation-table (just n) 1 sweep)
             (find_room space_data target_generation typemod n t)
             (memcpy_aligned t oldt n)))
         (set! (RELOCCODE t) _)
         (set! (CODERELOC _) t)])
      (S_record_code_mod tc_in (cast uptr (& (CODEIT _ 0))) (cast uptr (CODELEN _)))]
     [else])]))

(define-trace-macro (add-ephemeron-to-pending)
  (case-mode
   [sweep
    (add_ephemeron_to_pending _)]
   [measure
    (add_ephemeron_to_pending_measure _)]
   [else]))

(define-trace-macro (count-record rtd)
  (case-mode
   [copy
    (case-counts
     [keep-counts
      (let* ([c_rtd : ptr (cond
                            [(== tf _) _copy_]
                            [else rtd])]
             [counts : ptr (record-type-counts c_rtd)])
        (cond
          [(== counts Sfalse)
           (let* ([grtd : IGEN (GENERATION c_rtd)])
             (set! (array-ref (array-ref S_G.countof grtd) countof_rtd_counts) += 1)
             ;; Allocate counts struct in same generation as rtd. Initialize timestamp & counts.
             (find_room space_data grtd type_typed_object size_rtd_counts counts)
             (set! (rtd-counts-type counts) type_rtd_counts)
             (set! (rtd-counts-timestamp counts) (array-ref S_G.gctimestamp 0))
             (let* ([g : IGEN 0])
               (while
                :? (<= g static_generation)
                (set! (RTDCOUNTSIT counts g) 0)
                (set! g += 1)))
             (set! (record-type-counts c_rtd) counts)
             (set! (array-ref S_G.rtds_with_counts grtd)
                   (S_cons_in (cond [(== grtd 0) space_new] [else space_impure]) grtd c_rtd
                              (array-ref S_G.rtds_with_counts grtd)))
             (set! (array-ref (array-ref S_G.countof grtd) countof_pair) += 1))]
          [else
           (trace-early (just counts))
           (set! (record-type-counts c_rtd) counts)
           (when (!= (rtd-counts-timestamp counts) (array-ref S_G.gctimestamp 0))
             (S_fixup_counts counts))])
        (set! (RTDCOUNTSIT counts tg) (+ (RTDCOUNTSIT counts tg) 1)))]
     [no-counts])]
   [else]))

;; ----------------------------------------

(define-syntax (match stx)
  (syntax-case stx (else)
    [(_ expr [pattern rhs ...] ... [else else-rhs ...])
     #'(let ([v expr]) (matching v [pattern rhs ...] ... [else else-rhs ...]))]
    [(_ expr [pattern rhs ...] ...)
     #'(let ([v expr]) (match v [pattern rhs ...] ... [else (error 'match "no matching clause: ~s" v)]))]))

(define-syntax (matching stx)
  (syntax-case stx ()
    [(_ v [else rhs ...])
     #'(let () rhs ...)]
    [(_ v [pattern rhs ...] more ...)
     (letrec ([gen-match (lambda (pat quoted?)
                           (cond
                             [(identifier? pat)
                              (if quoted?
                                  #`(eq? v '#,pat)
                                  #t)]
                             [else
                              (syntax-case pat (quasiquote unquote)
                                [(quasiquote p)
                                 (if quoted?
                                     (error 'match "bad quasiquote")
                                     (gen-match #'p #t))]
                                [(unquote p)
                                 (if quoted?
                                     (gen-match #'p #f)
                                     (error 'match "bad unquote"))]
                                [(a . b)
                                 #`(and (pair? v)
                                        (let ([v (car v)])
                                          #,(gen-match #'a quoted?))
                                        (let ([v (cdr v)])
                                          #,(gen-match #'b quoted?)))]
                                [other
                                 #'(equal? v 'other)])]))]
              [get-binds (lambda (pat quoted?)
                           (cond
                             [(identifier? pat)
                              (if quoted?
                                  '()
                                  (list pat))]
                             [else
                              (syntax-case pat (quasiquote unquote)
                                [(quasiquote p)
                                 (get-binds #'p #t)]
                                [(unquote p)
                                 (get-binds #'p #f)]
                                [(a . b)
                                 (append (get-binds #'a quoted?)
                                         (get-binds #'b quoted?))]
                                [other '()])]))]
              [get-vals (lambda (pat quoted?)
                          (cond
                            [(identifier? pat)
                             (if quoted?
                                 #''()
                                 #'(list v))]
                            [else
                             (syntax-case pat (quasiquote unquote)
                               [(quasiquote p)
                                (get-vals #'p #t)]
                               [(unquote p)
                                (get-vals #'p #f)]
                               [(a . b)
                                #`(append (let ([v (car v)])
                                            #,(get-vals #'a quoted?))
                                          (let ([v (cdr v)])
                                            #,(get-vals #'b quoted?)))]
                               [other #''()])]))])
       (syntax-case #'pattern (quasiquote)
         [(quasiquote p)
          #`(if #,(gen-match #'pattern #f)
                (let-values ([#,(get-binds #'pattern #f)
                              (apply values #,(get-vals #'pattern #f))])
                  rhs ...)
                (matching v more ...))]
         [_
          (error 'match "bad pattern ~s" #'pattern)]))]))

(let ()

  (define preserve-flonum-eq? #t)
  
  ;; A spec is an association list with these possible keys:
  ;;   - 'mode [required]
  ;;   - 'backreferences
  ;;   - 'known-space [prunes generated cases]
  ;;   - 'known-types [prunes generated cases]

  (define-record-type seq
    (fields l))
  (define-record-type block-seq
    (fields l))
  (define-record-type indent-seq
    (fields pre mid post))

  (define lookup
    (case-lambda
     [(key spec default)
      (let ([a (assq key spec)])
        (if a
            (cadr a)
            default))]
     [(key spec)
      (let ([a (assq key spec)])
        (if a
            (cadr a)
            (error 'lookup "not found: ~s" key)))]))

  (define (code . l) (make-seq l))
  (define (code-block . l) (make-block-seq l))
  (define (code-indent pre mid post) (make-indent-seq pre mid post))

  (define (generate name spec)
    (define base-types (prune trace-base-types spec))
    (define object-types (prune trace-object-types spec))
    (define mode (lookup 'mode spec))
    (code
     (format "static ~a ~a(~aptr p~a)"
             (case (lookup 'mode spec)
               [(copy) "ptr"]
               [(size) "uptr"]
               [(self-test) "IBOOL"]
               [else "void"])
             name
             (case (lookup 'mode spec)
               [(sweep)
                (if (type-included? 'code spec)
                    "ptr tc_in, "
                    "")]
               [else ""])
             (case (lookup 'mode spec)
               [(copy) ", seginfo *si"]
               [else ""]))
     (let ([body
            (lambda ()
              (cond
                [(null? base-types)
                 (cond
                   [(null? object-types)
                    (error 'generate "no relevant types")]
                   [(null? (cdr object-types))
                    (code-block (generate-type-code (cdar object-types) spec))]
                   [else
                    (generate-object-dispatch object-types (cons '(basetype typed-object) spec))])]
                [else
                 (cond
                   [(null? object-types)
                    (generate-type-dispatch base-types spec)]
                   [else
                    (generate-type-dispatch 
                     (cons (cons 'typed-object
                                 (generate-object-dispatch object-types (cons '(basetype typed-object)
                                                                              spec)))
                           base-types)
                     spec)])]))])
       (case (lookup 'mode spec)
         [(copy)
          (code-block
           "if (locked(p)) return p;"
           "change = 1;"
           "check_triggers(si);"
           (code-block
            "ptr new_p;"
            "IGEN tg = target_generation;"
            (body)
            "FWDMARKER(p) = forward_marker;"
            "FWDADDRESS(p) = new_p;"
            (and (lookup 'backreferences spec #f)
                 "ADD_BACKREFERENCE(p)")
            "return new_p;"))]
         [(sweep)
          (code-block
           "PUSH_BACKREFERENCE(p)"
           (body)
           "POP_BACKREFERENCE()")]
         [(measure)
          (body)]
         [(self-test)
          (code-block
           (body)
           "return 0;")]
         [else
          (body)]))))

  (define (generate-type-dispatch l spec)
    (code-block
     "ITYPE t = TYPEBITS(p);"
     (let loop ([l l] [else? #f])
       (cond
         [(null? l)
          (code "else"
                (code-block
                 (format "S_error_abort(\"~a: illegal type\");" (lookup 'mode spec))))]
         [else
          (code
           (format "~aif (t == type_~a)" (if else? "else " "") (dehyphen (caar l)))
           (let ([c (cdar l)])
             (if (block-seq? c)
                 c
                 (code-block (generate-type-code c (cons (list 'basetype (caar l))
                                                         spec)))))
           (loop (cdr l) #t))]))))

  (define (generate-object-dispatch l spec)
    (code-block
     "ptr tf = TYPEFIELD(p);"
     (let loop ([l l] [else? #f])
       (cond
         [(null? l)
          (code "else"
                (code-block
                 (format "S_error_abort(\"~a: illegal typed object type\");" (lookup 'mode spec))))]
         [else
          (let* ([ty (caar l)]
                 [mask (lookup-constant (string->symbol (format "mask-~a" ty)))]
                 [type-constant? (eqv? mask (constant byte-constant-mask))])
            (code (format "~aif (~a)" (if else? "else " "")
                          (if type-constant?
                              (format "(iptr)tf == type_~a" (dehyphen ty))
                              (format "TYPEP(tf, mask_~a, type_~a)" (dehyphen ty) (dehyphen ty))))
                  (code-block (generate-type-code (cdar l) (cons (list 'tf "tf")
                                                                 (if type-constant?
                                                                     (cons `(type-constant ,(format "type_~a" (dehyphen ty)))
                                                                           spec)
                                                                     spec))))
                  (loop (cdr l) #t)))]))))

  (define (generate-type-code l spec)
    (cond
      [(null? l) (code)]
      [else
       (let ([a (car l)])
         (match a
           [`(case-mode . ,all-clauses)
            (let ([mode (lookup 'mode spec)])
              (let ([body (let loop ([clauses all-clauses])
                            (match clauses
                              [`([else . ,body])
                               body]
                              [`([,cl-mode . ,cl-body] . ,clauses)
                               (if (or (eq? cl-mode mode)
                                       (and (pair? cl-mode)
                                            (memq mode cl-mode)))
                                   cl-body
                                   (loop clauses))]
                              [`()
                               (error 'case-mode "no matching case for ~s in ~s" mode all-clauses)]))])
                (generate-type-code (append body (cdr l)) spec)))]
           [`(case-space . ,all-clauses)
            (code
             (code-block
              (format "ISPC p_at_spc = ~a;"
                      (case (lookup 'mode spec)
                        [(copy) "si->space"]
                        [else "SPACE(p) & ~(space_locked | space_old)"]))
              (let loop ([all-clauses all-clauses] [else? #f])
                (match all-clauses
                  [`([else . ,body])
                   (code
                    "else"
                    (code-block (generate-type-code body spec)))]
                  [`([,spc . ,body] . ,rest)
                   (code
                    (format "~aif (p_at_spc == ~a)"
                            (if else? "else " "")
                            (case (lookup 'mode spec)
                              [(copy) (format "(~a | space_old)" (dehyphen spc))]
                              [else (dehyphen spc)]))
                    (code-block (generate-type-code body spec))
                    (loop rest #t))])))
             (generate-type-code (cdr l) spec))]
           [`(case-backreferences
              [keep-backreferences . ,keep]
              [no-backreferences . ,no-keep])
            (code
             (if (lookup 'backreferences spec #f)
                 (generate-type-code keep spec)
                 (generate-type-code no-keep spec))
             (generate-type-code (cdr l) spec))]
           [`(case-counts
              [keep-counts . ,keep]
              [no-counts . ,no-keep])
            (code
             (if (lookup 'counts spec #f)
                 (generate-type-code keep spec)
                 (generate-type-code no-keep spec))
             (generate-type-code (cdr l) spec))]
           [`(trace-early-rtd ,field)
            (code (case (and (not (lookup 'only-dirty? spec #f))
                             (lookup 'mode spec))
                    [(copy sweep)
                     (code
                      "/* Relocate to make sure we aren't using an oldspace descriptor"
                      "   that has been overwritten by a forwarding marker, but don't loop"
                      "   on tag-reflexive base descriptor */"
                      (format "if (p != ~a)"
                              (lookup 'tf spec (format "TYPEFIELD(p)")))
                      (code-block
                       (generate-type-code `((trace-early ,field)) spec)))]
                    [(measure)
                     (generate-type-code `((trace-early ,field)) spec)]
                    [else #f])
                  (generate-type-code (cdr l) (cons `(copy-extra-rtd ,field) spec)))]
           [`(trace ,field)
            (code (trace-statement field spec #f)
                  (generate-type-code (cdr l) spec))]
           [`(trace-early ,field)
            (code (trace-statement field spec #t)
                  (generate-type-code (cdr l) (if (symbol? field)
                                                  (cons `(copy-extra ,field) spec)
                                                  spec)))]
           [`(trace-now ,field)
            (code
             (case (lookup 'mode spec)
               [(copy)
                (code-block
                 (format "ptr tmp_p = ~a;" (field-expression field spec "p" #f))
                 (relocate-statement "tmp_p")
                 (format "~a = tmp_p;" (field-expression field spec "new_p" #f)))]
               [(self-test) #f]
               [(measure)
                (generate-type-code (list `(trace ,field)) spec)]
               [else
                (trace-statement field spec #f)])
             (generate-type-code (cdr l) spec))]
           [`(copy ,field)
            (code (copy-statement field spec)
                  (generate-type-code (cdr l) spec))]
           [`(copy-flonum ,field)
            (cond
              [(and preserve-flonum-eq?
                    (eq? 'copy (lookup 'mode spec)))
               (code (copy-statement field spec)
                     "flonum_set_forwarded(p, si);"
                     "FLONUM_FWDADDRESS(p) = new_p;"
                     (generate-type-code (cdr l) spec))]
              [else
               (generate-type-code (cons `(copy ,field) (cdr l)) spec)])]
           [`(copy-flonum* ,field)
            (cond
              [preserve-flonum-eq?
               (case (lookup 'mode spec)
                 [(copy)
                  (code (code-block
                         (format "ptr tmp_p = TYPE(&~a, type_flonum);" (field-expression field spec "p" #t))
                         "if (flonum_is_forwarded_p(tmp_p, si))"
                         (format "  ~a = FLODAT(FLONUM_FWDADDRESS(tmp_p));"
                                 (field-expression field spec "new_p" #f))
                         "else"
                         (format "  ~a = ~a;"
                                 (field-expression field spec "new_p" #f)
                                 (field-expression field spec "p" #f)))
                        (generate-type-code (cdr l) spec))]
                 [else (generate-type-code (cdr l) spec)])]
              [else
               (generate-type-code (cons `(copy ,field) (cdr l)) spec)])]
           [`(copy-bytes ,offset ,len)
            (code (case (lookup 'mode spec)
                    [(copy) (format "memcpy_aligned(&~a, &~a, ~a);"
                                    (field-expression offset spec "new_p" #t)
                                    (field-expression offset spec "p" #t)
                                    (expression len spec))]
                    [else #f])
                  (generate-type-code (cdr l) spec))]
           [`(copy-type ,field)
            (case (lookup 'mode spec)
              [(copy)
               (code
                (format "~a = ~a;"
                        (field-expression field spec "new_p" #f)
                        (or (lookup 'type-constant spec #f)
                            "(uptr)tf"))
                (generate-type-code (cdr l) spec))]
              [else
               (generate-type-code (cons `(copy ,field) (cdr l)) spec)])]
           [`(trace-ptrs ,offset ,len)
            (case (lookup 'mode spec)
              [(copy)
               (generate-type-code (cons `(copy-bytes ,offset (* ptr_bytes ,len))
                                         (cdr l))
                                   spec)]
              [(sweep measure)
               (code
                (loop-over-pointers
                 (field-expression offset spec "p" #t)
                 len
                 (trace-statement `(array-ref p_p idx) spec #f)
                 spec))]
              [(self-test)
               (code
                (loop-over-pointers (field-expression offset spec "p" #t)
                                    len
                                    (code "if (p_p[idx] == p) return 1;")
                                    spec)
                (generate-type-code (cdr l) spec))]
              [else (generate-type-code (cdr l) spec)])]
           [`(count ,counter)
            (code (count-statement counter #f 1 'copy spec)
                  (generate-type-code (cdr l) spec))]
           [`(count ,counter ,size)
            (generate-type-code (cons `(count ,counter ,size 1 copy) (cdr l)) spec)]
           [`(count ,counter ,size ,scale)
            (generate-type-code (cons `(count ,counter ,size ,scale copy) (cdr l)) spec)]
           [`(count ,counter ,size ,scale ,mode)
            (code (count-statement counter size scale mode
                                   (cons `(constant-size? ,(symbol? size))
                                         spec))
                  (generate-type-code (cdr l) spec))]
           [`(space ,s)
            (case (lookup 'mode spec)
              [(copy)
               (code (code-indent "ISPC p_spc = "
                                  (expression s spec #f #t)
                                  ";")
                     (generate-type-code (cdr l) (cons '(space-ready? #t) spec)))]
              [else (generate-type-code (cdr l) spec)])]
           [`(size ,sz)
            (generate-type-code (cons `(size ,sz ,1) (cdr l)) spec)]
           [`(size ,sz ,scale)
            (let* ([mode (lookup 'mode spec)]
                   [mode (if (and (eq? mode 'sweep)
                                  (lookup 'return-size spec #f))
                             'sweep+size
                             mode)])
              (code-block
               (case mode
                 [(copy sweep+size size measure)
                  (format "uptr p_sz = ~a;" (let ([s (size-expression sz spec)])
                                              (if (= scale 1)
                                                  s
                                                  (format "~a * (~a)" scale s))))]
                 [else #f])
               (case mode
                 [(copy)
                  (unless (lookup 'space-ready? spec #f)
                    (error 'generate "size before space"))
                  (code (format "find_room(p_spc, tg, type_~a, p_sz, new_p);"
                                (dehyphen (lookup 'basetype spec)))
                        (generate-type-code (let ([extra (lookup 'copy-extra spec #f)])
                                              (if extra
                                                  (cons `(copy ,extra) (cdr l))
                                                  (let ([extra (and (eq? 'copy (lookup 'mode spec))
                                                                    (lookup 'copy-extra-rtd spec #f))])
                                                    (if extra
                                                        (cons `(set! (,extra _copy_)
                                                                     (cond
                                                                       [(== tf _) _copy_]
                                                                       [else rtd]))
                                                              (cdr l))
                                                        (cdr l)))))
                                            (cons '(copy-ready? #t)
                                                  (if (symbol? sz)
                                                      (cons '(constant-size? #t)
                                                            spec)
                                                      spec))))]
                 [(size)
                  (code "return p_sz;")]
                 [(measure)
                  (code "measure_total += p_sz;"
                        (generate-type-code (cdr l) spec))]
                 [else (generate-type-code (cdr l) spec)])))]
           [`(pad ,e)
            (case (lookup 'mode spec)
              [(copy)
               (generate-type-code (cons e (cdr l)) spec)]
              [else
               (generate-type-code (cdr l) spec)])]
           [`(skip-forwarding)
            (case (lookup 'mode spec)
              [(copy)
               (unless (null? (cdr l))
                 (error 'skip-forwarding "not at end"))
               (code "return new_p;")]
              [else
               (generate-type-code (cdr l) spec)])]
           [`(define ,id : ,type ,rhs)
            (code-block (code-indent (format "~a ~a = " type id)
                                     (expression rhs spec #f #t)
                                     ";")
                        (generate-type-code (cdr l) spec))]
           [`(cond . ,clauses)
            (code
             (let loop ([clauses clauses] [else? #f])
               (match clauses
                 [`() (code)]
                 [`([else . ,rhss])
                  (if else?
                      (code "else"
                            (code-block
                             (generate-type-code rhss spec)))
                      (generate-type-code rhss spec))]
                 [`([,test . ,rhss] . ,clauses)
                  (code (format "~aif (~a)" (if else? "else " "") (expression test spec))
                        (code-block
                         (generate-type-code rhss spec))
                        (loop clauses #t))]))
             (generate-type-code (cdr l) spec))]
           [`(let* ,binds . ,body)
            (code
             (code-block
              (let loop ([binds binds])
                (match binds
                  [`() (generate-type-code body spec)]
                  [`([,id : ,type ,rhs] . ,binds)
                   (code (code-indent (format "~a ~a = " type id)
                                      (expression rhs spec #f #t)
                                      ";")
                         (loop binds))])))
             (generate-type-code (cdr l) spec))]
           [`(while :? ,tst . ,body)
            (code (format "while (~a)"  (expression tst spec))
                  (code-block
                   (generate-type-code body spec))
                  (generate-type-code (cdr l) spec))]
           [`(do-while . ,body+test)
            (let-values ([(body tst)
                          (let loop ([body+test body+test] [rev-body '()])
                            (match body+test
                              [`(:? ,test) (values (reverse rev-body) test)]
                              [`(,e . ,rest)
                               (loop rest (cons e rev-body))]))])
              (code "do"
                    (code-block
                     (generate-type-code body spec))
                    (format "while (~a);"  (expression tst spec))
                    (generate-type-code (cdr l) spec)))]
           [`(when ,tst . ,body)
            (code (format "if (~a)"  (expression tst spec))
                  (code-block
                   (generate-type-code body spec))
                  (generate-type-code (cdr l) spec))]
           [`(set! ,lhs ,rhs)
            (code (code-indent (format "~a = "
                                       (expression lhs spec))
                               (expression rhs spec #f #t)
                               ";")
                  (generate-type-code (cdr l) spec))]
           [`(set! ,lhs ,op ,rhs)
            (unless (memq op '(+= -= <<= >>=))
              (error 'set! "not an update op ~s" op))
            (code (format "~a ~a ~a;"
                          (expression lhs spec)
                          op
                          (expression rhs spec))
                  (generate-type-code (cdr l) spec))]
           [`(,id . ,args)
            (let ([m (eq-hashtable-ref trace-macros id #f)])
              (if m
                  (generate-type-code (append (apply-macro m args)
                                              (cdr l))
                                      spec)
                  (code (format "~a;" (expression a spec #f #t))
                        (generate-type-code (cdr l) spec))))]
           [`break
            (code "break;")]
           [else
            (code (format "~a;" (expression a spec #f #t))
                  (generate-type-code (cdr l) spec))]))]))

  (define expression
    (case-lambda
     [(a spec) (expression a spec #f #f)]
     [(a spec protect?) (expression a spec protect? #f)]
     [(a spec protect? multiline?)
      (define (protect s)
        (if protect? (format "(~a)" s) s))
      (match a
        [`_ "p"]
        [`_copy_ (case (lookup 'mode spec)
                   [(copy) "new_p"]
                   [else "p"])]
        [`(just ,id) (symbol->string id)]
        [`(case-backreferences
           [keep-backreferences ,keep]
           [no-backreferences ,no-keep])
         (if (lookup 'backreferences spec #f)
             (if multiline?
                 (format "(BACKREFERENCES_ENABLED\n ? ~a\n : ~a)"
                         (indent-newlines (expression keep spec #t #t) 3)
                         (indent-newlines (expression no-keep spec #t #t) 3))
                 (format "(BACKREFERENCES_ENABLED ? ~a : ~a)"
                         (expression keep spec #t)
                         (expression no-keep spec #t)))
             (expression no-keep spec protect?))]
        [`(cond . ,clauses)
         (let loop ([clauses clauses] [protect? protect?])
           (match clauses
             [`([else ,rhs]) (expression rhs spec protect?)]
             [`([,test ,rhs] . ,clauses)
              (if multiline?
                  (format "(~a\n ? ~a\n : ~a)"
                          (expression test spec #t #t)
                          (indent-newlines (expression rhs spec #t #t) 3)
                          (indent-newlines (loop clauses #t) 3))
                  (format "(~a ? ~a : ~a)"
                          (expression test spec #t #t)
                          (expression rhs spec #t #t)
                          (loop clauses #t)))]))]
        [`(cast ,type ,e)
         (protect (format "(~a)~a" type (expression e spec #t)))]
        [`(array-ref ,array ,index)
         (protect (format "~a[~a]"
                          (expression array spec #t)
                          (expression index spec)))]
        [`(set! ,lhs ,rhs) ; a `set!` used as an expression
         (format "(~a = ~a)"
                 (expression lhs spec #t)
                 (expression rhs spec #t))]
        [`(,op ,a)
         (cond
           [(memq op '(& - !))
            (protect (format "~a~a" op (expression a spec #t)))]
           [(get-offset-value op)
            => (lambda (v)
                 (protect (field-ref-expression (expression a spec) v op)))]
           [else
            (protect (format "~a(~a)" op (expression a spec #t)))])]
        [`(,op ,a ,b)
         (if (memq op '(& && \|\| == != + - * < > <= >= << >> ->))
             (protect (format "~a ~a ~a" (expression a spec #t) op (expression b spec #t)))
             (protect (format "~a(~a, ~a)" op (expression a spec) (expression b spec))))]
        [`(,rator . ,rands)
         (format "~a(~a)"
                 rator
                 (comma-ize (map (lambda (r) (expression r spec)) rands)))]
        [else
         (cond
           [(symbol? a) (dehyphen a)]
           [else
            (format "~s" a)])])]))

  (define (loop-over-pointers ptr-e len body spec)
    (code-block
     (format "uptr idx, p_len = ~a;" (expression len spec))
     (format "ptr *p_p = &~a;" ptr-e)
     "for (idx = 0; idx < p_len; idx++)"
     (code-block body)))

  (define (trace-statement field spec early?)
    (define mode (lookup 'mode spec))
    (cond
      [(or (eq? mode 'sweep)
           (and early? (eq? mode 'copy)))
       (relocate-statement (field-expression field spec "p" #t))]
      [(eq? mode 'copy)
       (copy-statement field spec)]
      [(eq? mode 'measure)
       (measure-statement (field-expression field spec "p" #f))]
      [(eq? mode 'self-test)
       (format "if (p == ~a) return 1;" (field-expression field spec "p" #f))]
      [else #f]))

  (define (relocate-statement e)
    (format "relocate(&~a);" e))

  (define (measure-statement e)
    (code
     "{ /* measure */"
     (format "  ptr r_p = ~a;" e)
     "  if (!IMMEDIATE(r_p))"
     "    push_measure(r_p);"
     "}"))

  (define (copy-statement field spec)
    (define mode (lookup 'mode spec))
    (cond
      [(eq? mode 'copy)
       (unless (lookup 'copy-ready? spec #f)
         (error 'copy "need size before: ~s" field))
       (format "~a = ~a;"
               (field-expression field spec "new_p" #f)
               (field-expression field spec "p" #f))]
      [else #f]))

  (define (count-statement counter size scale mode spec)
    (cond
      [(eq? mode (lookup 'mode spec))
       (cond
         [(lookup 'counts spec #f)
          (let ([tg (if (eq? mode 'copy)
                        "tg"
                        "target_generation")])
            (code
             (format "S_G.countof[~a][~a] += ~a;" tg (dehyphen counter) scale)
             (if (lookup 'constant-size? spec #f)
                 #f
                 (format "S_G.bytesof[~a][~a] += ~a;"
                         tg
                         (dehyphen counter)
                         (let ([s (if size
                                      (expression size spec)
                                      "p_sz")])
                           (if (eqv? scale 1)
                               s
                               (format "~a * (~a)" scale s)))))))]
         [else #f])]
      [else #f]))

  (define (field-expression field spec arg protect?)
    (if (symbol? field)
        (cond
          [(get-offset-value field)
           => (lambda (v)
                (field-ref-expression arg v field))]
          [else
           (error 'field "identifier is not a field accessor: ~s" field)])
        (expression field spec protect?)))

  (define (size-expression sz spec)
    (if (symbol? sz)
        (cond
          [(get-size-value sz)
           => (lambda (v) (dehyphen sz))]
          [else
           (error 'size "identifier is not a size: ~s" sz)])
        (expression sz spec)))

  (define (field-ref-expression obj v acc-name)
    (let ([c-ref (getprop acc-name '*c-ref* #f)])
      (if c-ref
          (if (pair? c-ref)
              (format "~a(~a,0)" (car c-ref) obj)
              (format "~a(~a)" c-ref obj))
          (format "FIELDREF(~a, ~a /* ~a */)" obj v acc-name))))
  
  (define (get-offset-value op)
    (getprop (string->symbol (format "~a-disp" op)) '*constant* #f))

  (define (get-size-value op)
    (getprop op '*constant* #f))

  (define (comma-ize l)
    (apply string-append
           (let loop ([l l])
             (if (null? l)
                 '("")
                 (if (null? (cdr l))
                     (list (car l))
                     (list* (car l) ", " (loop (cdr l))))))))

  (define (dehyphen s)
    (list->string
     (map (lambda (c)
            (if (eqv? c #\-)
                #\_
                c))
          (string->list (symbol->string s)))))

  (define (apply-macro m l)
    (define args (car m))
    (define body (cdr m))
    (unless (= (length args) (length l))
      (error 'apply-macro "wrong macro argument count: ~s vs ~s" args l))
    (let ([subs (map cons args l)])
      (let loop ([m body])
        (cond
          [(symbol? m)
           (let ([a (assq m subs)])
             (if a
                 (cdr a)
                 m))]
          [(pair? m)
           (cons (loop (car m)) (loop (cdr m)))]
          [else m]))))

  (define (type-included? type spec)
    (let ([types (lookup 'known-types spec #f)])
      (if (not types)
          #t
          (memq type types))))

  (define (prune types spec)
    (let loop ([types types])
      (if (null? types)
          '()
          (let ([s (prune-one (car types) spec)])
            (if s
                (cons s (loop (cdr types)))
                (loop (cdr types)))))))

  (define (prune-one type spec)
    (define known-types (lookup 'known-types spec #f))
    (cond
      [(or (not known-types)
           (memq (car type) known-types))
       (let ([known-space (lookup 'known-space spec #f)])
         (cond
           [(or (not known-space)
                (body-has-space? (cdr type) known-space spec))
            type]
           [else #f]))]
      [else #f]))

  (define (body-has-space? body space spec)
    (cond
      [(null? body) (error 'base-has-space? "no `space` specification in body")]
      [else
       (let ([a (car body)])
         (cond
           [(and (pair? a) (eq? (car a) 'space))
            (body-has-tail? (cdr a) space spec)]
           [(and (pair? a) (memq (car a) '(case-space cond)))
            (unless (null? (cdr body)) (error 'body-has-space? "there's more?"))
            (let loop ([clauses (cdr a)])
              (if (null? clauses)
                  #f
                  (or (body-has-space? (cdar clauses) space spec)
                      (loop (cdr clauses)))))]
           [else
            (body-has-space? (cdr body) space spec)]))]))

  (define (body-has-tail? body key spec)
    (cond
      [(null? body) #f]
      [else
       (let ([a (car body)])
         (match a
           [`(case-backreferences
              [keep-backreferences ,keep]
              [no-backreferences ,no-keep])
            (if (lookup 'backreferences spec #f)
                (or (body-has-tail? keep key spec)
                    (body-has-tail? no-keep key spec))
                (body-has-tail? no-keep key spec))]
           [`(cond . ,clauses)
            (ormap (lambda (clause)
                     (body-has-tail? (cdr clause) key spec))
                   clauses)]
           [else
            (body-has-tail? (cdr body) key spec)]))]))

  (define print-code
    (case-lambda
     [(c)
      (print-code c 0)
      (newline)]
     [(c indentation)
      (cond
        [(not c) (void)]
        [(seq? c)
         (for-each (lambda (p)
                     (print-code p indentation))
                   (seq-l c))]
        [(block-seq? c)
         (let ([l (block-seq-l c)])
           (cond
             [(and (pair? l)
                   (null? (cdr l))
                   (block-seq? (car l)))
              (print-code (car l) indentation)]
             [else
              (indent indentation)
              (printf "{\n")
              (for-each (lambda (p)
                          (print-code p (+ indentation 2)))
                        l)
              (indent indentation)
              (printf "}\n")]))]
        [(indent-seq? c)
         (indent indentation)
         (printf "~a" (indent-seq-pre c))
         (printf "~a" (indent-newlines (indent-seq-mid c)
                                       (+ indentation (string-length (indent-seq-pre c)))))
         (printf "~a" (indent-seq-post c))
         (newline)]
        [else
         (indent indentation)
         (printf "~a\n" (indent-newlines c indentation))])]))

  (define (indent n)
    (display (make-string n #\space)))

  (define (indent-newlines s n)
    (list->string
     (let loop ([l (string->list s)])
       (cond
         [(null? l) '()]
         [(eqv? #\newline (car l))
          (cons #\newline (append (string->list (make-string n #\space))
                                  (loop (cdr l))))]
         [else (cons (car l) (loop (cdr l)))]))))

  (define (gen ofn count?)
    (parameterize ([current-output-port (open-output-file ofn 'replace)])
      (print-code (generate "copy"
                            `((mode copy)
                              (backreferences ,count?)
                              (counts ,count?))))
      (print-code (generate "sweep"
                            `((mode sweep)
                              (backreferences ,count?)
                              (counts ,count?))))
      (print-code (generate "sweep_record"
                            `((mode sweep)
                              (known-types (record))
                              (backreferences ,count?)
                              (counts ,count?))))
      (print-code (generate "size_object"
                            `((mode size))))
      (print-code (generate "object_directly_refers_to_self"
                            `((mode self-test))))))
  
  (set! mkgc-ocd.inc (lambda (ofn) (gen ofn #f)))
  (set! mkgc-oce.inc (lambda (ofn) (gen ofn #t))))

"hamt"
;; Based on the "hamt" package by Jon Zeppieri

(let ()
  (define-record-type bnode
    (fields vector bitmap)
    (nongenerative)
    (sealed #t))
  (define-record-type cnode
    (fields vector hashcode)
    (nongenerative)
    (sealed #t))

  (define *nothing* (list '*nothing*))

  (define empty-bnode (make-bnode '#() 0))

  (define hamt? bnode?)

  (define (hamt-empty? h)
    (fx= (vector-length (bnode-vector h)) 0))

  (define (hamt-ref h key key-num key= default)
    (node-ref h key (key-num key) key= 0 default))

  (define (hamt-set h key key-num key= val)
    (node-set h key val (key-num key) key= key-num 0))

  (define (hamt-remove h key key-num key=)
    (node-remove h key (key-num key) key= 0))

  (define (hamt-map h proc)
    (hamt-fold h '() (lambda (k v acc) (cons (proc k v) acc))))

  (define (hamt-keys h)
    (hamt-fold h '() (lambda (k _ acc) (cons k acc))))

  (define (hamt-values h)
    (hamt-fold h '() (lambda (_ v acc) (cons v acc))))

  (define (hamt->list h)
    (hamt-fold h '() (lambda (k v acc) (cons (cons k v) acc))))

  (define (hamt-for-each h proc)
    (hamt-fold h (void) (lambda (k v _) (proc k v) (void))))

  (define (hamt-fold h id proc)
    (node-fold h id proc))
  
  (define (node-ref node key keyhash key= shift default)
    (cond
     [(bnode? node) (bnode-ref node key keyhash key= shift default)]
     [(cnode? node) (cnode-ref node key keyhash key= default)]
     [else ($oops 'node-ref "[BUG] node-ref: unknown node type")]))

  (define (node-set node key val keyhash key= key-num shift)
    (cond
     [(bnode? node) (bnode-set node key val keyhash key= key-num shift)]
     [(cnode? node) (cnode-set node key val keyhash key= key-num shift)]
     [else ($oops 'node-set "[BUG] node-set: unknown node type")]))

  (define (node-remove node key keyhash key= shift)
    (cond
     [(bnode? node) (bnode-remove node key keyhash key= shift)]
     [(cnode? node) (cnode-remove node key keyhash key= shift)]
     [else ($oops 'node-remove "[BUG] node-remove: unknown node type")]))

  (define (node-fold n acc proc)
    (cond
     [(bnode? n) (vector-fold (bnode-vector n) acc proc)]
     [(cnode? n) (vector-fold (cnode-vector n) acc proc)]
     [else ($oops 'node-fold "[BUG] node-fold: unknown node type")]))

  (define (vector-fold arr acc proc)
    (let ([len (vector-length arr)])
      (let loop ([acc acc] [i 0])
        (cond
         [(fx= i len) acc]
         [else
          (let ([key (vector-ref arr i)]
                [val (vector-ref arr (fx1+ i))])
            (if (eq? *nothing* val)
                (loop (node-fold key acc proc) (fx+ 2 i))
                (loop (proc key val acc) (fx+ 2 i))))]))))

  (define (bnode-ref node key keyhash key= shift default)
    (let ([idx (bnode-index node keyhash shift)])
      (cond
       [(not idx) default]
       [else
        (let ([arr (bnode-vector node)])
          (let ([k (vector-key-ref arr idx)]
                [v (vector-val-ref arr idx)])
            (cond
             [(not (eq? v *nothing*))
              (cond
               [(key= key k) v]
               [else default])]
             [else (node-ref k key keyhash key= (down shift) default)])))])))

  (define (cnode-ref node key keyhash key= default)
    (let ([v (cnode-vector-ref node key keyhash key=)])
      (cond
       [(eq? v *nothing*) default]
       [else v])))

  (define (vector-key-ref arr idx)
    (vector-ref arr (fx+ idx idx)))
  (define (vector-val-ref arr idx)
    (vector-ref arr (fx+ 1 idx idx)))

  (define (bnode-set node key val keyhash key= key-num shift)
    (let* ([arr (bnode-vector node)]
           [bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)]
           [idx (bnode-idx bitmap bit)])
      (cond
       [(bit-set? bitmap bit)
        (let ([k (vector-key-ref arr idx)]
              [v (vector-val-ref arr idx)])
          (cond
           [(not (eq? v *nothing*))
            (cond
             [(key= key k)
              (if (eq? v val)
                  node
                  (make-bnode (vector-replace arr idx key val)
                              bitmap))]
             [else
              (let ([child (make-node k v key val keyhash key= key-num (down shift))])
                (make-bnode (vector-replace arr idx child *nothing*) bitmap))])]
           [else
            (let ([new-child (node-set k key val keyhash key= key-num (down shift))])
              (if (eq? new-child k)
                  node
                  (make-bnode (vector-replace arr idx new-child *nothing*) bitmap)))]))]
       [else
        (make-bnode (vector-insert arr idx key val)
                    (fxior bitmap (fxsll 1 bit)))])))

  (define (cnode-set node key val keyhash key= key-num shift)
    (let* ([arr (cnode-vector node)]
           [hashcode (cnode-hashcode node)])
      (cond
       [(= hashcode keyhash)
        (let ([idx (cnode-index arr key key=)])
          (cond
           [idx
            (make-cnode (vector-replace arr idx key val) hashcode)]
           [else (make-cnode (vector-insert arr 0 key val) hashcode)]))]
       [else
        (let ([new (make-bnode (vector node *nothing*) (fxsll 1 (bnode-bit hashcode shift)))])
          (node-set new key val keyhash key= key-num shift))])))

  (define (bnode-remove node key keyhash key= shift)
    (let* ([arr (bnode-vector node)]
           [bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)]
           [idx (bnode-idx bitmap bit)])
      (cond
       [(bit-set? bitmap bit)
        (let ([k (vector-key-ref arr idx)]
              [v (vector-val-ref arr idx)])
          (cond
           [(not (eq? v *nothing*))
            (cond
             [(key= key k)
              (let ([new-arr (vector-remove arr idx)])
                (cond
                 [(contract-node? new-arr shift)
                  (vector-ref new-arr 0)]
                 [else
                  (make-bnode new-arr (fxxor bitmap (fxsll 1 bit)))]))]
             [else
              node])]
           [else
            (let* ([child k]
                   [new-child (node-remove child key keyhash key= (down shift))])
              (cond
               [(eq? child new-child)
                node]
               [else
                (let ([new-arr (vector-replace arr idx new-child *nothing*)])
                  (cond
                   [(contract-node? new-arr shift)
                    (vector-ref new-arr 0)]
                   [else
                    (make-bnode new-arr bitmap)]))]))]))]
       [else node])))

  (define (cnode-remove node key keyhash key= shift)
    (let ([arr (cnode-vector node)]
          [hashcode (cnode-hashcode node)])
      (cond
       [(= hashcode keyhash)
        (let ([idx (cnode-index arr key key=)])
          (cond
           [idx
            (let ([new-arr (vector-remove arr idx)])
              (cond
               [(contract-node? new-arr shift)
                (vector-ref new-arr 0)]
               [else
                (make-cnode new-arr hashcode)]))]
           [else node]))]
       [else node])))

  (define (cnode-vector-ref node key keyhash key=)
    (let ([arr (cnode-vector node)]
          [hashcode (cnode-hashcode node)])
      (if (= hashcode keyhash)
          (let ([i (cnode-index arr key key=)])
            (if i
                (vector-val-ref arr i)
                *nothing*))
          *nothing*)))

  (define (cnode-index arr key key=)
    (let ([len (vector-length arr)])
      (let loop ([i 0])
        (cond
         [(fx= i len) #f]
         [else
          (let ([k (vector-ref arr i)])
            (if (key= key k)
                (fxsra i 1)
                (loop (fx+ 2 i))))]))))

  (define (make-node k1 v1 k2 v2 k2hash key= key-num shift)
    (let ([k1hash (key-num k1)])
      (cond
       [(= k1hash k2hash)
        (make-cnode (vector k1 v1 k2 v2) k1hash)]
       [else
        (let ([n (node-set empty-bnode k1 v1 k1hash key= key-num shift)])
          (node-set n k2 v2 k2hash key= key-num shift))])))

  (define (contract-node? arr shift)
    (and (fx= (vector-length arr) 2)
         (fx> shift 0)
         (not (eq? *nothing* (vector-ref arr 1)))))

  (define (bnode-index node keyhash shift)
    (let* ([bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)])
      (and (bit-set? bitmap bit)
           (bnode-idx bitmap bit))))

  (define (bnode-bit keyhash shift)
    (fxand (fxsra keyhash shift) #x0f))

  (define (bnode-idx bitmap bit)
    (fxbit-count (fxand bitmap (fx- (fxsll 1 bit) 1))))

  (define (bit-set? bitmap bit)
    (fxlogbit? bit bitmap))

  (define (down shift)
    (fx+ shift 4))

  (define (vector-replace arr idx key val)
    (let* ([len (vector-length arr)]
           [new (make-vector len)]
           [j (fx+ idx idx)])
      (vector-copy! new 0 arr 0 j)
      (vector-set! new j key)
      (vector-set! new (fx1+ j) val)
      (vector-copy! new (fx+ 2 j) arr (fx+ 2 j) len)
      new))

  (define (vector-insert arr idx key val)
    (let ([new (make-vector (fx+ (vector-length arr) 2))]
          [j (fx+ idx idx)])
      (vector-copy! new 0 arr 0 j)
      (vector-set! new j key)
      (vector-set! new (fx1+ j) val)
      (vector-copy! new (fx+ j 2) arr j (vector-length arr))
      new))

  (define (vector-remove arr idx)
    (let ([new (make-vector (fx- (vector-length arr) 2))]
          [j (fx+ idx idx)])
      (vector-copy! new 0 arr 0 j)
      (vector-copy! new j arr (fx+ j 2) (vector-length arr))
      new))

  (define vector-copy!
    (case-lambda
     [(dest dest-start src)
      (vector-copy! dest dest-start src 0 (vector-length src))]
     [(src src-start dest dest-start)
      (vector-copy! dest dest-start src src-start (vector-length src))]
     [(dest dest-start src src-start src-end)
      (let loop ([i (- src-end src-start)])
        (unless (zero? i)
          (let ([i (sub1 i)])
            (vector-set! dest (+ dest-start i) (vector-ref src (+ src-start i)))
            (loop i))))]))

  (set! $hamt-empty (lambda () empty-bnode))
  (set! $hamt-set hamt-set)
  (set! $hamt-ref hamt-ref))

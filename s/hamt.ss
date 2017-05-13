"hamt"
;; Based on the "hamt" package by Jon Zeppieri

(let ()
  (define-record-type entry
    (fields key value)
    (nongenerative)
    (sealed #t))
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
          (let ([x (vector-ref arr i)])
            (if (entry? x)
                (loop (proc (entry-key x) (entry-value x) acc) (fx1+ i))
                (loop (node-fold x acc proc) (fx1+ i))))]))))

  (define (node-iterate n k done-k)
    (cond
     [(bnode? n) (vector-iterate (bnode-vector n) k done-k)]
     [(cnode? n) (vector-iterate (cnode-vector n) k done-k)]
     [else ($oops 'node-fold "[BUG] node-fold: unknown node type")]))

  (define (vector-iterate arr k done-k)
    (let ([len (vector-length arr)])
      (let loop ([i 0] [k k])
        (cond
         [(fx= i len) (done-k k)]
         [else
          (let ([x (vector-ref arr i)])
            (if (entry? x)
                (k x (lambda (k) (loop (fx1+ i) k)))
                (node-iterate x k (lambda (k) (loop (fx1+ i) k)))))]))))

  (define (bnode-ref node key keyhash key= shift default)
    (let ([e (bnode-vector-ref node keyhash shift)])
      (cond
       [(not e) default]
       [(entry? e)
        (let ([k (entry-key e)]
              [v (entry-value e)])
          (cond
           [(key= key k) v]
           [else default]))]
       [else (node-ref e key keyhash key= (down shift) default)])))

  (define (cnode-ref node key keyhash key= default)
    (let ([e (cnode-vector-ref node key keyhash key=)])
      (cond
       [(entry? e) (entry-value e)]
       [else default])))

  (define (bnode-set node key val keyhash key= key-num shift)
    (let* ([arr (bnode-vector node)]
           [bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)]
           [idx (bnode-idx bitmap bit)])
      (cond
       [(bit-set? bitmap bit)
        (let ([e (vector-ref arr idx)])
          (cond
           [(entry? e)
            (let ([k (entry-key e)]
                  [v (entry-value e)])
              (cond
               [(key= key k)
                (if (eq? v val)
                    node
                    (make-bnode (vector-replace arr idx (make-entry key val))
                                bitmap))]
               
               [else
                (let ([child (make-node k v key val keyhash key= key-num (down shift))])
                  (make-bnode (vector-replace arr idx child) bitmap))]))]
           [else
            (let ([new-child (node-set e key val keyhash key= key-num (down shift))])
              (if (eq? new-child e)
                  node
                  (make-bnode (vector-replace arr idx new-child) bitmap)))]))]
       [else
        (make-bnode (vector-insert arr idx (make-entry key val))
                    (fxior bitmap (fxsll 1 bit)))])))

  (define (cnode-set node key val keyhash key= key-num shift)
    (let* ([arr (cnode-vector node)]
           [hashcode (cnode-hashcode node)])
      (cond
       [(= hashcode keyhash)
        (let ([idx (cnode-index arr key key=)])
          (cond
           [idx
            (make-cnode (vector-replace arr idx (make-entry key val)) hashcode)]
           [else (make-cnode (vector-insert arr (vector-length arr) (make-entry key val)) hashcode)]))]
       [else
        (let ([new (make-bnode (vector node) (fxsll 1 (bnode-bit hashcode shift)))])
          (node-set new key val keyhash key= key-num shift))])))

  (define (bnode-remove node key keyhash key= shift)
    (let* ([arr (bnode-vector node)]
           [bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)]
           [idx (bnode-idx bitmap bit)])
      (cond
       [(bit-set? bitmap bit)
        (let ([e (vector-ref arr idx)])
          (cond
           [(entry? e)
            (let ([k (entry-key e)])
              (cond
               [(key= key k)
                (let ([new-arr (vector-remove arr idx)])
                  (cond
                   [(contract-node? new-arr shift)
                    (vector-ref new-arr 0)]
                   [else
                    (make-bnode new-arr (fxxor bitmap (fxsll 1 bit)))]))]
               [else
                node]))]
           [else
            (let* ([child e]
                   [new-child (node-remove child key keyhash key= (down shift))])
              (cond
               [(eq? child new-child)
                node]
               [else
                (let ([new-arr (vector-replace arr idx new-child)])
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
      (and (= hashcode keyhash)
           (let ([i (cnode-index arr key key=)])
             (and i (vector-ref arr i))))))

  (define (cnode-index arr key key=)
    (let ([len (vector-length arr)])
      (let loop ([i 0])
        (cond
         [(fx= i len) #f]
         [else
          (let ([e (vector-ref arr i)])
            (if (key= key (entry-key e))
                i
                (loop (fx1+ i))))]))))

  (define (make-node k1 v1 k2 v2 k2hash key= key-num shift)
    (let ([k1hash (key-num k1)])
      (cond
       [(= k1hash k2hash)
        (make-cnode (vector (make-entry k1 v1) (make-entry k2 v2)) k1hash)]
       [else
        (let ([n (node-set empty-bnode k1 v1 k1hash key= key-num shift)])
          (node-set n k2 v2 k2hash key= key-num shift))])))

  (define (contract-node? arr shift)
    (and (fx= (vector-length arr) 1)
         (fx> shift 0)
         (entry? (vector-ref arr 0))))

  (define (bnode-vector-ref node keyhash shift)
    (let* ([arr (bnode-vector node)]
           [bitmap (bnode-bitmap node)]
           [bit (bnode-bit keyhash shift)])
      (cond
       [(bit-set? bitmap bit)
        (vector-ref arr (bnode-idx bitmap bit))]
       [else
        #f])))

  (define (bnode-bit keyhash shift)
    (fxand (fxsra keyhash shift) #x0f))

  (define (bnode-idx bitmap bit)
    (fxbit-count (fxand bitmap (fx- (fxsll 1 bit) 1))))

  (define (bit-set? bitmap bit)
    (fxlogbit? bit bitmap))

  (define (down shift)
    (fx+ shift 4))

    (define (vector-replace arr idx val)
    (let* ([len (vector-length arr)]
           [new (make-vector len)])
      (let loop ([i 0])
        (cond
         [(fx= i idx)
          (vector-set! new i val)
          (loop (fx+ i 1))]
         [(fx< i len)
          (vector-set! new i (vector-ref arr i))
          (loop (fx+ i 1))]
         [else
          new]))))

  (define (vector-insert arr idx val)
    (let ([new (make-vector (fx+ (vector-length arr) 1))])
      (vector-copy! new 0 arr 0 idx)
      (vector-set! new idx val)
      (vector-copy! new (fx+ idx 1) arr idx (vector-length arr))
      new))

  (define (vector-remove arr idx)
    (let ([new (make-vector (fx- (vector-length arr) 1))])
      (vector-copy! new 0 arr 0 idx)
      (vector-copy! new idx arr (fx+ idx 1) (vector-length arr))
      new))

  (define vector-copy!
    (case-lambda
     [(dest dest-start src)
      (vector-copy! dest dest-start src 0
                    (if (vector? src) (vector-length src) 0))]
     [(src src-start dest dest-start)
      (vector-copy! dest dest-start src src-start
                    (if (vector? src) (vector-length src) 0))]
     [(dest dest-start src src-start src-end)
      (let loop ([i (- src-end src-start)])
        (unless (zero? i)
          (let ([i (sub1 i)])
            (vector-set! dest (+ dest-start i) (vector-ref src (+ src-start i)))
            (loop i))))]))

  (set! $hamt-empty (lambda () empty-bnode))
  (set! $hamt-set hamt-set)
  (set! $hamt-ref hamt-ref))

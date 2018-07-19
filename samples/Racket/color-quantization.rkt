#lang racket/base
(require racket/class
         racket/draw)

;; This is an implementation of the Octree Quantization algorithm.  This implementation
;; follows the sketch in:
;;
;; Dean Clark.  Color Quantization using Octrees.  Dr. Dobbs Portal, January 1, 1996.
;; http://www.ddj.com/184409805
;;
;; This code is adapted from the color quantizer in the implementation of Racket's
;; file/gif standard library.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; To view an example of the quantizer, run the following test submodule
;; in DrRacket:
(module+ test
  (require racket/block net/url)

  (define frog
    (block
      (define url (string->url "http://rosettacode.org/mw/images/3/3f/Quantum_frog.png"))
      (define frog-ip (get-pure-port url))
      (define bitmap (make-object bitmap% frog-ip))
      (close-input-port frog-ip)
      bitmap))

  ;; Display the original:
  (print frog)
  ;; And the quantized version (16 colors):
  (print (quantize-bitmap frog 16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; quantize-bitmap: bitmap positive-number -> bitmap
;; Given a bitmap, returns a new bitmap quantized to, at most, n colors.
(define (quantize-bitmap bm n)
  (let* ([width (send bm get-width)]
         [height (send bm get-height)]
         [len (* width height 4)]
         [source-buffer (make-bytes len)]
         [_ (send bm get-argb-pixels 0 0 width height source-buffer)]
         [an-octree (make-octree-from-argb source-buffer n)]
         [dest-buffer (make-bytes len)])
    (let quantize-bitmap-loop ([i 0])
      (when (< i len)
        (let* ([i+1 (+ i 1)]
               [i+2 (+ i 2)]
               [i+3 (+ i 3)]
               [a (bytes-ref source-buffer i)]
               [r (bytes-ref source-buffer i+1)]
               [g (bytes-ref source-buffer i+2)]
               [b (bytes-ref source-buffer i+3)])
          (cond
            [(alpha-opaque? a)
             (let-values ([(new-r new-g new-b)
                           (octree-lookup an-octree r g b)])
               (bytes-set! dest-buffer i 255)
               (bytes-set! dest-buffer i+1 new-r)
               (bytes-set! dest-buffer i+2 new-g)
               (bytes-set! dest-buffer i+3 new-b))]
            [else
             (bytes-set! dest-buffer i 0)
             (bytes-set! dest-buffer i+1 0)
             (bytes-set! dest-buffer i+2 0)
             (bytes-set! dest-buffer i+3 0)]))
        (quantize-bitmap-loop (+ i 4))))
    (let* ([new-bm (make-object bitmap% width height)]
           [dc (make-object bitmap-dc% new-bm)])
      (send dc set-argb-pixels 0 0 width height dest-buffer)
      (send dc set-bitmap #f)
      new-bm)))





;; make-octree-from-argb: bytes positive-number -> octree
;; Constructs an octree ready to quantize the colors from an-argb.
(define (make-octree-from-argb an-argb n)
  (unless (> n 0)
    (raise-type-error 'make-octree-from-argb "positive number" n))
  (let ([an-octree (new-octree)]
        [len (bytes-length an-argb)])
    (let make-octree-loop ([i 0])
      (when (< i len)
        (let ([a (bytes-ref an-argb i)]
              [r (bytes-ref an-argb (+ i 1))]
              [g (bytes-ref an-argb (+ i 2))]
              [b (bytes-ref an-argb (+ i 3))])
          (when (alpha-opaque? a)
            (octree-insert-color! an-octree r g b)
            (let reduction-loop ()
              (when (> (octree-leaf-count an-octree) n)
                (octree-reduce! an-octree)
                (reduction-loop)))))
        (make-octree-loop (+ i 4))))
    (octree-finalize! an-octree)
    an-octree))


;; alpha-opaque? byte -> boolean
;; Returns true if the alpha value is considered opaque.
(define (alpha-opaque? a)
  (>= a 128))



;; The maximum level height of an octree.
(define MAX-LEVEL 7)



;; A color is a (vector byte byte byte)

;; An octree is a:
(define-struct octree (root            ; node
                       leaf-count      ; number
                       reduction-heads ; (vectorof (or/c node #f))
                       palette)        ; (vectorof (or/c color #f))
  #:mutable)
;; reduction-heads is used to accelerate the search for a reduction candidate.


;; A subtree node is a:
(define-struct node (leaf?          ; bool
                     npixels        ; number  -- number of pixels this subtree node represents
                     redsum         ; number
                     greensum       ; number
                     bluesum        ; number
                     children       ; (vectorof (or/c #f node))
                     next           ; (or/c #f node)
                     palette-index) ; (or/c #f byte?)
  #:mutable)
;; node-next is used to accelerate the search for a reduction candidate.


;; new-octree: -> octree
(define (new-octree)
  (let* ([root-node (make-node #f ;; not a leaf
                               0  ;; no pixels under us yet
                               0  ;; red sum
                               0  ;; green sum
                               0  ;; blue sum
                               (make-vector 8 #f) ;; no children so far
                               #f ;; next
                               #f ;; palette-index
                               )]
         [an-octree
          (make-octree root-node
                       0 ; no leaves so far
                       (make-vector (add1 MAX-LEVEL) #f) ; no reductions so far
                       (make-vector 256 #(0 0 0)))])        ; the palette
    ;; Although we'll almost never reduce to this level, initialize the first
    ;; reducible node to the root, for completeness sake.
    (vector-set! (octree-reduction-heads an-octree) 0 root-node)
    an-octree))


;; rgb->index: natural-number byte byte byte -> octet
;; Given a level and an (r,g,b) triplet, returns an octet that can be used
;; as an index into our octree structure.
(define (rgb->index level r g b)
  (bitwise-ior (bitwise-and 4 (arithmetic-shift r (- level 5)))
               (bitwise-and 2 (arithmetic-shift g (- level 6)))
               (bitwise-and 1 (arithmetic-shift b (- level 7)))))


;; octree-insert-color!: octree byte byte byte -> void
;; Accumulates a new r,g,b triplet into the octree.
(define (octree-insert-color! an-octree r g b)
  (node-insert-color! (octree-root an-octree) an-octree r g b 0))


;; node-insert-color!: node octree byte byte byte natural-number -> void
;; Adds a color to the node subtree.  While we hit #f, we create new nodes.
;; If we hit an existing leaf, we accumulate our color into it.
(define (node-insert-color! a-node an-octree r g b level)
  (let insert-color-loop ([a-node a-node]
                          [level level])
    (cond [(node-leaf? a-node)
           ;; update the leaf with the new color
           (set-node-npixels! a-node (add1 (node-npixels a-node)))
           (set-node-redsum! a-node (+ (node-redsum a-node) r))
           (set-node-greensum! a-node (+ (node-greensum a-node) g))
           (set-node-bluesum! a-node (+ (node-bluesum a-node) b))]
          [else
           ;; create the child node if necessary
           (let ([index (rgb->index level r g b)])
             (unless (vector-ref (node-children a-node) index)
               (let ([new-node (make-node (= level MAX-LEVEL) ; leaf?
                                          0  ; npixels
                                          0  ; redsum
                                          0  ; greensum
                                          0  ; bluesum
                                          (make-vector 8 #f) ; no children yet
                                          #f ; and no next node yet
                                          #f ; or palette index
                                          )])
                 (vector-set! (node-children a-node) index new-node)
                 (cond
                   [(= level MAX-LEVEL)
                    ;; If we added a leaf, mark it in the octree.
                    (set-octree-leaf-count! an-octree
                                            (add1 (octree-leaf-count an-octree)))]
                   [else
                    ;; Attach the node as a reducible node if it's interior.
                    (set-node-next!
                     new-node (vector-ref (octree-reduction-heads an-octree)
                                          (add1 level)))
                    (vector-set! (octree-reduction-heads an-octree)
                                 (add1 level)
                                 new-node)])))
             ;; and recur on the child node.
             (insert-color-loop (vector-ref (node-children a-node) index)
                                (add1 level)))])))


;; octree-reduce!: octree -> void
;; Reduces one of the subtrees, collapsing the children into a single node.
(define (octree-reduce! an-octree)
  (node-reduce! (pop-reduction-candidate! an-octree) an-octree))


;; node-reduce!: node octree -> void
;; Reduces the interior node.
(define (node-reduce! a-node an-octree)
  (for ([child (in-vector (node-children a-node))]
        #:when child)
    (set-node-npixels! a-node (+ (node-npixels a-node)
                                 (node-npixels child)))
    (set-node-redsum! a-node (+ (node-redsum a-node)
                                (node-redsum child)))
    (set-node-greensum! a-node (+ (node-greensum a-node)
                                  (node-greensum child)))
    (set-node-bluesum! a-node (+ (node-bluesum a-node)
                                 (node-bluesum child)))
    (set-octree-leaf-count! an-octree (sub1 (octree-leaf-count an-octree))))
  (set-node-leaf?! a-node #t)
  (set-octree-leaf-count! an-octree (add1 (octree-leaf-count an-octree))))


;; find-reduction-candidate!: octree -> node
;; Returns a bottom-level interior node for reduction.  Also takes the
;; candidate out of the conceptual queue of reduction candidates.
(define (pop-reduction-candidate! an-octree)
  (let loop ([i MAX-LEVEL])
    (cond
      [(vector-ref (octree-reduction-heads an-octree) i)
       =>
       (lambda (candidate-node)
         (when (> i 0)
           (vector-set! (octree-reduction-heads an-octree) i
                        (node-next candidate-node)))
         candidate-node)]
      [else
       (loop (sub1 i))])))


;; octree-finalize!: octree -> void
;; Finalization does a few things:
;; * Walks through the octree and reduces any interior nodes with just one leaf child.
;;   Optimizes future lookups.
;; * Fills in the palette of the octree and the palette indexes of the leaf nodes.
;; * Note: palette index 0 is always reserved for the transparent color.
(define (octree-finalize! an-octree)
  ;; Collapse one-leaf interior nodes.
  (let loop ([a-node (octree-root an-octree)])
    (for ([child (in-vector (node-children a-node))]
          #:when (and child (not (node-leaf? child))))
      (loop child)
      (when (interior-node-one-leaf-child? a-node)
        (node-reduce! a-node an-octree))))

  ;; Attach palette entries.
  (let ([current-palette-index 1])
    (let loop ([a-node (octree-root an-octree)])
      (cond [(node-leaf? a-node)
             (let ([n (node-npixels a-node)])
               (vector-set! (octree-palette an-octree) current-palette-index
                            (vector (quotient (node-redsum a-node) n)
                                    (quotient (node-greensum a-node) n)
                                    (quotient (node-bluesum a-node) n)))
               (set-node-palette-index! a-node current-palette-index)
               (set! current-palette-index (add1 current-palette-index)))]
            [else
             (for ([child (in-vector (node-children a-node))]
                   #:when child)
               (loop child))]))))


;; interior-node-one-leaf-child?: node -> boolean
(define (interior-node-one-leaf-child? a-node)
  (let ([child-list (filter values (vector->list (node-children a-node)))])
    (and (= (length child-list) 1)
         (node-leaf? (car child-list)))))


;; octree-lookup: octree byte byte byte -> (values byte byte byte)
;; Returns the palettized color.
(define (octree-lookup an-octree r g b)
  (let* ([index (node-lookup-index (octree-root an-octree) an-octree r g b 0)]
         [vec (vector-ref (octree-palette an-octree) index)])
    (values (vector-ref vec 0)
            (vector-ref vec 1)
            (vector-ref vec 2))))



;; node-lookup-index: node byte byte byte natural-number -> byte
;; Returns the palettized color index.
(define (node-lookup-index a-node an-octree r g b level)
  (let loop ([a-node a-node]
             [level level])
    (if (node-leaf? a-node)
      (node-palette-index a-node)
      (let ([child (vector-ref (node-children a-node) (rgb->index level r g b))])
        (unless child
          (error 'node-lookup-index
                 "color (~a, ~a, ~a) not previously inserted"
                 r g b))
        (loop child (add1 level))))))

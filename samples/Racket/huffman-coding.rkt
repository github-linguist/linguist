#lang racket

(require data/heap
         data/bit-vector)

;; A node is either an interior, or a leaf.
;; In either case, they record an item with an associated frequency.
(struct node (freq) #:transparent)
(struct interior node (left right) #:transparent)
(struct leaf node (val) #:transparent)

;; node<=?: node node -> boolean
;; Compares two nodes by frequency.
(define (node<=? x y)
  (<= (node-freq x) (node-freq y)))

;; We keep a private sentinel-val under our own control.
(define sentinel-val (cons 'sentinel 'sentinel))

;; make-huffman-tree: (listof leaf) -> interior-node
;; Makes the huffman tree with basic priority-queue operations.
;; Note: we ensure that make-huffman-tree always returns an interior node.
(define (make-huffman-tree leaves)
  (define a-heap (make-heap node<=?))
  (heap-add-all! a-heap leaves)
  ;; To ensure that we always get tree with at least one interior node,
  ;; we also inject a sentinel leaf node with zero frequency.
  (heap-add! a-heap (leaf 0 sentinel-val))
  (for ([i (in-range (length leaves))])
    (define min-1 (heap-min a-heap))
    (heap-remove-min! a-heap)
    (define min-2 (heap-min a-heap))
    (heap-remove-min! a-heap)
    (heap-add! a-heap (interior (+ (node-freq min-1) (node-freq min-2))
                                min-1 min-2)))
  (heap-min a-heap))

;; string->huffman-tree: string -> node
;; Given a string, produces its huffman tree.  The leaves hold the characters
;; and their relative frequencies.
(define (string->huffman-tree str)
  (define ht (make-hash))
  (define n (sequence-length str))
  (for ([ch str])
    (hash-set! ht ch (add1 (hash-ref ht ch 0))))
  (make-huffman-tree
   (for/list ([(k v) (in-hash ht)])
     (leaf (/ v n) k))))

;; make-encoder: node -> (string -> bit-vector)
;; Given a huffman tree, generates the encoder function.
(define (make-encoder a-tree)
  (define dict (huffman-tree->dictionary a-tree))
  (lambda (a-str)
    (list->bit-vector (apply append (for/list ([ch a-str]) (hash-ref dict ch))))))

;; huffman-tree->dictionary: node -> (hashof val (listof boolean))
;; A helper for the encoder: maps characters to their code sequences.
(define (huffman-tree->dictionary a-node)
  (define ht (make-hash))
  (let loop ([a-node a-node]
             [path/rev '()])
    (cond
      [(interior? a-node)
       (loop (interior-left a-node) (cons #f path/rev))
       (loop (interior-right a-node) (cons #t path/rev))]
      [(leaf? a-node)
       (unless (eq? (leaf-val a-node) sentinel-val)
         (hash-set! ht (reverse path/rev) (leaf-val a-node)))]))
  (for/hash ([(k v) ht])
    (values v k)))

;; make-decoder: interior-node -> (bit-vector -> string)
;; Generates the decoder function from the tree.
(define (make-decoder a-tree)
  (lambda (a-bitvector)
    (define-values (decoded/rev _)
      (for/fold ([decoded/rev '()]
                 [a-node a-tree])
                ([bit a-bitvector])
        (define next-node
          (cond
            [(not bit)
             (interior-left a-node)]
            [else
             (interior-right a-node)]))
        (cond [(leaf? next-node)
               (values (cons (leaf-val next-node) decoded/rev)
                       a-tree)]
              [else
               (values decoded/rev next-node)])))
    (apply string (reverse decoded/rev))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example application:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msg "this is an example for huffman encoding")

(define tree (string->huffman-tree msg))

;; We can print out the mapping for inspection:
(huffman-tree->dictionary tree)

(define encode (make-encoder tree))
(define encoded (encode msg))

;; Here's what the encoded message looks like:
(bit-vector->string encoded)

(define decode (make-decoder tree))
;; Here's what the decoded message looks like:
(decode encoded)

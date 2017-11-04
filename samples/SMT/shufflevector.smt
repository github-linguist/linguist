(set-logic QF_ABV)
(define-sort Address () (_ BitVec 64))
(define-sort Byte () (_ BitVec 8))
(define-sort Mem () (Array Address Byte))

(define-sort I8 () (_ BitVec 8))
(define-sort I16 () (_ BitVec 16))
(define-sort I32 () (_ BitVec 32))
(define-sort I64 () (_ BitVec 64))
(define-sort I128 () (_ BitVec 128))


;;
;;constants
;;
(define-fun zero  () Address (_ bv0 64))
(define-fun one   () Address (_ bv1 64))
(define-fun two   () Address (_ bv2 64))
(define-fun three () Address (_ bv3 64))
(define-fun four  () Address (_ bv4 64))
(define-fun five  () Address (_ bv5 64))
(define-fun six   () Address (_ bv6 64))
(define-fun seven () Address (_ bv7 64))
(define-fun eight () Address (_ bv8 64))


;;
;; Write a little endian 1 bit value (8 bit aligned) at address x in mem
;;
(define-fun write1 ((mem Mem) (x Address) (v Bool)) Mem
  (store mem x (ite v #x01 #x00)))

;;
;; Write a little endian 8bit value at address x in mem
;;
(define-fun write8 ((mem Mem) (x Address) (v I8)) Mem
  (store mem x v))

;;
;; Write a little endian 16bit value at address x in mem
;;
(define-fun write16 ((mem Mem) (x Address) (v I16)) Mem
  (let ((b0 ((_ extract 7 0) v))
	(b1 ((_ extract 15 8) v)))
    (store (store mem x b0) (bvadd x one) b1)))

;;
;; Write a little endian 32bit value at address x in mem
;;
(define-fun write32 ((mem Mem) (x Address) (v I32)) Mem
  (let ((b0 ((_ extract 7 0) v))
	(b1 ((_ extract 15 8) v))
	(b2 ((_ extract 23 16) v))
	(b3 ((_ extract 31 24) v)))
    (store (store (store (store mem x b0) (bvadd x one) b1) (bvadd x two) b2) (bvadd x three) b3)))

;;
;; Write a little endian 64bit value at address x in mem
;;
(define-fun write64 ((mem Mem) (x Address) (v I64)) Mem
  (let ((b0 ((_ extract 31 0) v))
	(b1 ((_ extract 63 32) v)))
    (write32 (write32 mem x b0) (bvadd x four) b1)))

;;
;; Write a little endian 128bit value at address x in mem
;;
(define-fun write128 ((mem Mem) (x Address) (v I128)) Mem
  (let ((b0 ((_ extract 63 0) v))
	(b1 ((_ extract 127 64) v)))
    (write64 (write64 mem x b0) (bvadd x eight) b1)))


;;
;; Read a little endian 1 bit value (8 bit aligned) at address x in mem
;; - returns a Boolean: true if what's stored at address x is non-zero
;;
(define-fun read1 ((mem Mem) (x Address)) Bool
   (not (= (select mem x) #x00)))

;;
;; Read a little endian 8bit value at address x in mem
;;
(define-fun read8 ((mem Mem) (x Address)) I8
  (select mem x))

;;
;; Read a little endian 16bit value at address x in mem
;;
(define-fun read16 ((mem Mem) (x Address)) I16
  (let ((b0 (select mem x))
	(b1 (select mem (bvadd x one))))
    (concat b1 b0))) 

;;
;; Read a little endian 32bit value at address x in mem
;;
(define-fun read32 ((mem Mem) (x Address)) I32
  (let ((b0 (select mem x))
	(b1 (select mem (bvadd x one)))
	(b2 (select mem (bvadd x two)))
	(b3 (select mem (bvadd x three))))
    (concat b3 (concat b2 (concat b1 b0)))))

;;
;; Read a little endian 64bit value at address x in mem
;;
(define-fun read64 ((mem Mem) (x Address)) I64
  (let ((b0 (read32 mem x))
        (b1 (read32 mem (bvadd x four))))
    (concat b1 b0)))

;;
;; Read a little endian 128bit value at address x in mem
;;
(define-fun read128 ((mem Mem) (x Address)) I128
  (let ((b0 (read64 mem x))
        (b1 (read64 mem (bvadd x eight))))
    (concat b1 b0)))


;;
;; Vectors of (2^1) int4 elements
;;
(define-sort vector_1_4 () (Array (_ BitVec 1) (_ BitVec 4)))

(declare-fun vundef_1_4 () vector_1_4)


;;
;; Vectors of (2^1) int8 elements
;;
(define-sort vector_1_8 () (Array (_ BitVec 1) (_ BitVec 8)))

(declare-fun vundef_1_8 () vector_1_8)


;;
;; Vectors of (2^1) int16 elements
;;
(define-sort vector_1_16 () (Array (_ BitVec 1) (_ BitVec 16)))

(declare-fun vundef_1_16 () vector_1_16)


;;
;; Vectors of (2^1) int32 elements
;;
(define-sort vector_1_32 () (Array (_ BitVec 1) (_ BitVec 32)))

(declare-fun vundef_1_32 () vector_1_32)


;;
;; Vectors of (2^1) int64 elements
;;
(define-sort vector_1_64 () (Array (_ BitVec 1) (_ BitVec 64)))

(declare-fun vundef_1_64 () vector_1_64)


;;
;; Vectors of (2^2) int4 elements
;;
(define-sort vector_2_4 () (Array (_ BitVec 2) (_ BitVec 4)))

(declare-fun vundef_2_4 () vector_2_4)


;;
;; Vectors of (2^2) int8 elements
;;
(define-sort vector_2_8 () (Array (_ BitVec 2) (_ BitVec 8)))

(declare-fun vundef_2_8 () vector_2_8)


;;
;; Vectors of (2^2) int16 elements
;;
(define-sort vector_2_16 () (Array (_ BitVec 2) (_ BitVec 16)))

(declare-fun vundef_2_16 () vector_2_16)


;;
;; Vectors of (2^2) int32 elements
;;
(define-sort vector_2_32 () (Array (_ BitVec 2) (_ BitVec 32)))

(declare-fun vundef_2_32 () vector_2_32)


;;
;; Vectors of (2^2) int64 elements
;;
(define-sort vector_2_64 () (Array (_ BitVec 2) (_ BitVec 64)))

(declare-fun vundef_2_64 () vector_2_64)


;;
;; Vectors of (2^3) int4 elements
;;
(define-sort vector_3_4 () (Array (_ BitVec 3) (_ BitVec 4)))

(declare-fun vundef_3_4 () vector_3_4)


;;
;; Vectors of (2^3) int8 elements
;;
(define-sort vector_3_8 () (Array (_ BitVec 3) (_ BitVec 8)))

(declare-fun vundef_3_8 () vector_3_8)


;;
;; Vectors of (2^3) int16 elements
;;
(define-sort vector_3_16 () (Array (_ BitVec 3) (_ BitVec 16)))

(declare-fun vundef_3_16 () vector_3_16)


;;
;; Vectors of (2^3) int32 elements
;;
(define-sort vector_3_32 () (Array (_ BitVec 3) (_ BitVec 32)))

(declare-fun vundef_3_32 () vector_3_32)


;;
;; Vectors of (2^3) int64 elements
;;
(define-sort vector_3_64 () (Array (_ BitVec 3) (_ BitVec 64)))

(declare-fun vundef_3_64 () vector_3_64)

;; Special case where we use Bool rather than bitvectors of size 1 

(define-sort vector_1_1 () (Array (_ BitVec 1) Bool))

(declare-fun vundef_1_1 () vector_1_1)

(define-fun vmake_1_1 ((x0 Bool) (x1 Bool)) vector_1_1
   (store (store vundef_1_1 #b0 x0) #b1 x1))

(define-sort vector_2_1 () (Array (_ BitVec 2) Bool))

(declare-fun vundef_2_1 () vector_2_1)

(define-fun vmake_2_1 
  ((x0 Bool) (x1 Bool) (x2 Bool) (x3 Bool)) vector_2_1
   (store (store (store (store vundef_2_1 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

(define-sort vector_3_1 () (Array (_ BitVec 3) Bool))

(declare-fun vundef_3_1 () vector_3_1)

(define-fun vmake_3_1 
  ((x0 Bool) (x1 Bool) (x2 Bool) (x3 Bool) (x4 Bool) (x5 Bool) (x6 Bool) (x7 Bool)) vector_3_1
   (store (store (store (store (store (store (store (store vundef_3_1 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;;
;; Vectors of (2^1) i.e. two int4 elements
;;
(define-fun vmake_1_4 ((x0 (_ BitVec 4)) (x1 (_ BitVec 4))) vector_1_4
   (store (store vundef_1_4 #b0 x0) #b1 x1))

;;
;; Vectors of (2^2) i.e. four int4 elements
;;
(define-fun vmake_2_4 
  ((x0 (_ BitVec 4)) (x1 (_ BitVec 4)) (x2 (_ BitVec 4)) (x3 (_ BitVec 4))) vector_2_4
   (store (store (store (store vundef_2_4 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

;;
;; Vectors of (2^3) i.e. eight int4 elements
;;
(define-fun vmake_3_4 
  ((x0 (_ BitVec 4)) (x1 (_ BitVec 4)) (x2 (_ BitVec 4)) (x3 (_ BitVec 4))(x4 (_ BitVec 4)) (x5 (_ BitVec 4)) (x6 (_ BitVec 4)) (x7 (_ BitVec 4))) vector_3_4
   (store (store (store (store (store (store (store (store vundef_3_4 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;; zero vectors with int4 elements

 (define-fun vzero_1_4 () vector_1_4 (vmake_1_4 (_ bv0 4) (_ bv0 4)))

 (define-fun vzero_2_4 () vector_2_4 (vmake_2_4 (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4)))
 
 (define-fun vzero_3_4 () vector_3_4 (vmake_3_4 (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4) (_ bv0 4)))
 

;;
;; Vectors of (2^1) i.e. two int8 elements
;;
(define-fun vmake_1_8 ((x0 (_ BitVec 8)) (x1 (_ BitVec 8))) vector_1_8
   (store (store vundef_1_8 #b0 x0) #b1 x1))

;;
;; Vectors of (2^2) i.e. four int8 elements
;;
(define-fun vmake_2_8 
  ((x0 (_ BitVec 8)) (x1 (_ BitVec 8)) (x2 (_ BitVec 8)) (x3 (_ BitVec 8))) vector_2_8
   (store (store (store (store vundef_2_8 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

;;
;; Vectors of (2^3) i.e. eight int8 elements
;;
(define-fun vmake_3_8 
  ((x0 (_ BitVec 8)) (x1 (_ BitVec 8)) (x2 (_ BitVec 8)) (x3 (_ BitVec 8))(x4 (_ BitVec 8)) (x5 (_ BitVec 8)) (x6 (_ BitVec 8)) (x7 (_ BitVec 8))) vector_3_8
   (store (store (store (store (store (store (store (store vundef_3_8 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;; zero vectors with int8 elements

 (define-fun vzero_1_8 () vector_1_8 (vmake_1_8 (_ bv0 8) (_ bv0 8)))

 (define-fun vzero_2_8 () vector_2_8 (vmake_2_8 (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8)))
 
 (define-fun vzero_3_8 () vector_3_8 (vmake_3_8 (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8) (_ bv0 8)))
 

;;
;; Vectors of (2^1) i.e. two int16 elements
;;
(define-fun vmake_1_16 ((x0 (_ BitVec 16)) (x1 (_ BitVec 16))) vector_1_16
   (store (store vundef_1_16 #b0 x0) #b1 x1))

;;
;; Vectors of (2^2) i.e. four int16 elements
;;
(define-fun vmake_2_16 
  ((x0 (_ BitVec 16)) (x1 (_ BitVec 16)) (x2 (_ BitVec 16)) (x3 (_ BitVec 16))) vector_2_16
   (store (store (store (store vundef_2_16 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

;;
;; Vectors of (2^3) i.e. eight int16 elements
;;
(define-fun vmake_3_16 
  ((x0 (_ BitVec 16)) (x1 (_ BitVec 16)) (x2 (_ BitVec 16)) (x3 (_ BitVec 16))(x4 (_ BitVec 16)) (x5 (_ BitVec 16)) (x6 (_ BitVec 16)) (x7 (_ BitVec 16))) vector_3_16
   (store (store (store (store (store (store (store (store vundef_3_16 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;; zero vectors with int16 elements

 (define-fun vzero_1_16 () vector_1_16 (vmake_1_16 (_ bv0 16) (_ bv0 16)))

 (define-fun vzero_2_16 () vector_2_16 (vmake_2_16 (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16)))
 
 (define-fun vzero_3_16 () vector_3_16 (vmake_3_16 (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16) (_ bv0 16)))
 

;;
;; Vectors of (2^1) i.e. two int32 elements
;;
(define-fun vmake_1_32 ((x0 (_ BitVec 32)) (x1 (_ BitVec 32))) vector_1_32
   (store (store vundef_1_32 #b0 x0) #b1 x1))

;;
;; Vectors of (2^2) i.e. four int32 elements
;;
(define-fun vmake_2_32 
  ((x0 (_ BitVec 32)) (x1 (_ BitVec 32)) (x2 (_ BitVec 32)) (x3 (_ BitVec 32))) vector_2_32
   (store (store (store (store vundef_2_32 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

;;
;; Vectors of (2^3) i.e. eight int32 elements
;;
(define-fun vmake_3_32 
  ((x0 (_ BitVec 32)) (x1 (_ BitVec 32)) (x2 (_ BitVec 32)) (x3 (_ BitVec 32))(x4 (_ BitVec 32)) (x5 (_ BitVec 32)) (x6 (_ BitVec 32)) (x7 (_ BitVec 32))) vector_3_32
   (store (store (store (store (store (store (store (store vundef_3_32 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;; zero vectors with int32 elements

 (define-fun vzero_1_32 () vector_1_32 (vmake_1_32 (_ bv0 32) (_ bv0 32)))

 (define-fun vzero_2_32 () vector_2_32 (vmake_2_32 (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32)))
 
 (define-fun vzero_3_32 () vector_3_32 (vmake_3_32 (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32) (_ bv0 32)))
 

;;
;; Vectors of (2^1) i.e. two int64 elements
;;
(define-fun vmake_1_64 ((x0 (_ BitVec 64)) (x1 (_ BitVec 64))) vector_1_64
   (store (store vundef_1_64 #b0 x0) #b1 x1))

;;
;; Vectors of (2^2) i.e. four int64 elements
;;
(define-fun vmake_2_64 
  ((x0 (_ BitVec 64)) (x1 (_ BitVec 64)) (x2 (_ BitVec 64)) (x3 (_ BitVec 64))) vector_2_64
   (store (store (store (store vundef_2_64 #b00 x0) #b01 x1) #b10 x2) #b11 x3))

;;
;; Vectors of (2^3) i.e. eight int64 elements
;;
(define-fun vmake_3_64 
  ((x0 (_ BitVec 64)) (x1 (_ BitVec 64)) (x2 (_ BitVec 64)) (x3 (_ BitVec 64))(x4 (_ BitVec 64)) (x5 (_ BitVec 64)) (x6 (_ BitVec 64)) (x7 (_ BitVec 64))) vector_3_64
   (store (store (store (store (store (store (store (store vundef_3_64 #b000 x0) #b001 x1) #b010 x2) #b011 x3) #b100 x4) #b101 x5) #b110 x6) #b111 x7))


;; zero vectors with int64 elements

 (define-fun vzero_1_64 () vector_1_64 (vmake_1_64 (_ bv0 64) (_ bv0 64)))

 (define-fun vzero_2_64 () vector_2_64 (vmake_2_64 (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64)))
 
 (define-fun vzero_3_64 () vector_3_64 (vmake_3_64 (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64) (_ bv0 64)))
 

(define-fun vbvadd_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvadd (select x #b0) (select y #b0)))
         (z1 (bvadd (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvadd_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvadd (select x #b0) (select y #b0)))
         (z1 (bvadd (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvadd_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvadd (select x #b0) (select y #b0)))
         (z1 (bvadd (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvadd_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvadd (select x #b0) (select y #b0)))
         (z1 (bvadd (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvsub_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvsub (select x #b0) (select y #b0)))
         (z1 (bvsub (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvsub_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvsub (select x #b0) (select y #b0)))
         (z1 (bvsub (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvsub_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvsub (select x #b0) (select y #b0)))
         (z1 (bvsub (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvsub_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvsub (select x #b0) (select y #b0)))
         (z1 (bvsub (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvmul_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvmul (select x #b0) (select y #b0)))
         (z1 (bvmul (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvmul_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvmul (select x #b0) (select y #b0)))
         (z1 (bvmul (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvmul_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvmul (select x #b0) (select y #b0)))
         (z1 (bvmul (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvmul_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvmul (select x #b0) (select y #b0)))
         (z1 (bvmul (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvshl_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvshl (select x #b0) (select y #b0)))
         (z1 (bvshl (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvshl_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvshl (select x #b0) (select y #b0)))
         (z1 (bvshl (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvshl_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvshl (select x #b0) (select y #b0)))
         (z1 (bvshl (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvshl_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvshl (select x #b0) (select y #b0)))
         (z1 (bvshl (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvsdiv_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvsdiv (select x #b0) (select y #b0)))
         (z1 (bvsdiv (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvsdiv_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvsdiv (select x #b0) (select y #b0)))
         (z1 (bvsdiv (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvsdiv_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvsdiv (select x #b0) (select y #b0)))
         (z1 (bvsdiv (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvsdiv_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvsdiv (select x #b0) (select y #b0)))
         (z1 (bvsdiv (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvudiv_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvudiv (select x #b0) (select y #b0)))
         (z1 (bvudiv (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvudiv_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvudiv (select x #b0) (select y #b0)))
         (z1 (bvudiv (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvudiv_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvudiv (select x #b0) (select y #b0)))
         (z1 (bvudiv (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvudiv_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvudiv (select x #b0) (select y #b0)))
         (z1 (bvudiv (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvlshr_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvlshr (select x #b0) (select y #b0)))
         (z1 (bvlshr (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvlshr_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvlshr (select x #b0) (select y #b0)))
         (z1 (bvlshr (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvlshr_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvlshr (select x #b0) (select y #b0)))
         (z1 (bvlshr (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvlshr_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvlshr (select x #b0) (select y #b0)))
         (z1 (bvlshr (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvashr_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvashr (select x #b0) (select y #b0)))
         (z1 (bvashr (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvashr_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvashr (select x #b0) (select y #b0)))
         (z1 (bvashr (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvashr_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvashr (select x #b0) (select y #b0)))
         (z1 (bvashr (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvashr_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvashr (select x #b0) (select y #b0)))
         (z1 (bvashr (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvurem_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvurem (select x #b0) (select y #b0)))
         (z1 (bvurem (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvurem_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvurem (select x #b0) (select y #b0)))
         (z1 (bvurem (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvurem_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvurem (select x #b0) (select y #b0)))
         (z1 (bvurem (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvurem_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvurem (select x #b0) (select y #b0)))
         (z1 (bvurem (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvsrem_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvsrem (select x #b0) (select y #b0)))
         (z1 (bvsrem (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvsrem_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvsrem (select x #b0) (select y #b0)))
         (z1 (bvsrem (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvsrem_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvsrem (select x #b0) (select y #b0)))
         (z1 (bvsrem (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvsrem_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvsrem (select x #b0) (select y #b0)))
         (z1 (bvsrem (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvand_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvand (select x #b0) (select y #b0)))
         (z1 (bvand (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvand_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvand (select x #b0) (select y #b0)))
         (z1 (bvand (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvand_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvand (select x #b0) (select y #b0)))
         (z1 (bvand (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvand_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvand (select x #b0) (select y #b0)))
         (z1 (bvand (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvor_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvor (select x #b0) (select y #b0)))
         (z1 (bvor (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvor_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvor (select x #b0) (select y #b0)))
         (z1 (bvor (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvor_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvor (select x #b0) (select y #b0)))
         (z1 (bvor (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvor_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvor (select x #b0) (select y #b0)))
         (z1 (bvor (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

(define-fun vbvxor_1_8 ((x vector_1_8) (y vector_1_8)) vector_1_8
   (let ((z0 (bvxor (select x #b0) (select y #b0)))
         (z1 (bvxor (select x #b1) (select y #b1))))
      (vmake_1_8 z0 z1)))

(define-fun vbvxor_1_16 ((x vector_1_16) (y vector_1_16)) vector_1_16
   (let ((z0 (bvxor (select x #b0) (select y #b0)))
         (z1 (bvxor (select x #b1) (select y #b1))))
      (vmake_1_16 z0 z1)))

(define-fun vbvxor_1_32 ((x vector_1_32) (y vector_1_32)) vector_1_32
   (let ((z0 (bvxor (select x #b0) (select y #b0)))
         (z1 (bvxor (select x #b1) (select y #b1))))
      (vmake_1_32 z0 z1)))

(define-fun vbvxor_1_64 ((x vector_1_64) (y vector_1_64)) vector_1_64
   (let ((z0 (bvxor (select x #b0) (select y #b0)))
         (z1 (bvxor (select x #b1) (select y #b1))))
      (vmake_1_64 z0 z1)))

 (define-fun vbvadd_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvadd (select x #b00) (select y #b00)))
         (z1 (bvadd (select x #b01) (select y #b01)))
         (z2 (bvadd (select x #b10) (select y #b10)))
         (z3 (bvadd (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvadd_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvadd (select x #b00) (select y #b00)))
         (z1 (bvadd (select x #b01) (select y #b01)))
         (z2 (bvadd (select x #b10) (select y #b10)))
         (z3 (bvadd (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvadd_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvadd (select x #b00) (select y #b00)))
         (z1 (bvadd (select x #b01) (select y #b01)))
         (z2 (bvadd (select x #b10) (select y #b10)))
         (z3 (bvadd (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvadd_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvadd (select x #b00) (select y #b00)))
         (z1 (bvadd (select x #b01) (select y #b01)))
         (z2 (bvadd (select x #b10) (select y #b10)))
         (z3 (bvadd (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvsub_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvsub (select x #b00) (select y #b00)))
         (z1 (bvsub (select x #b01) (select y #b01)))
         (z2 (bvsub (select x #b10) (select y #b10)))
         (z3 (bvsub (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvsub_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvsub (select x #b00) (select y #b00)))
         (z1 (bvsub (select x #b01) (select y #b01)))
         (z2 (bvsub (select x #b10) (select y #b10)))
         (z3 (bvsub (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvsub_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvsub (select x #b00) (select y #b00)))
         (z1 (bvsub (select x #b01) (select y #b01)))
         (z2 (bvsub (select x #b10) (select y #b10)))
         (z3 (bvsub (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvsub_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvsub (select x #b00) (select y #b00)))
         (z1 (bvsub (select x #b01) (select y #b01)))
         (z2 (bvsub (select x #b10) (select y #b10)))
         (z3 (bvsub (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvmul_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvmul (select x #b00) (select y #b00)))
         (z1 (bvmul (select x #b01) (select y #b01)))
         (z2 (bvmul (select x #b10) (select y #b10)))
         (z3 (bvmul (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvmul_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvmul (select x #b00) (select y #b00)))
         (z1 (bvmul (select x #b01) (select y #b01)))
         (z2 (bvmul (select x #b10) (select y #b10)))
         (z3 (bvmul (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvmul_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvmul (select x #b00) (select y #b00)))
         (z1 (bvmul (select x #b01) (select y #b01)))
         (z2 (bvmul (select x #b10) (select y #b10)))
         (z3 (bvmul (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvmul_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvmul (select x #b00) (select y #b00)))
         (z1 (bvmul (select x #b01) (select y #b01)))
         (z2 (bvmul (select x #b10) (select y #b10)))
         (z3 (bvmul (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvshl_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvshl (select x #b00) (select y #b00)))
         (z1 (bvshl (select x #b01) (select y #b01)))
         (z2 (bvshl (select x #b10) (select y #b10)))
         (z3 (bvshl (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvshl_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvshl (select x #b00) (select y #b00)))
         (z1 (bvshl (select x #b01) (select y #b01)))
         (z2 (bvshl (select x #b10) (select y #b10)))
         (z3 (bvshl (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvshl_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvshl (select x #b00) (select y #b00)))
         (z1 (bvshl (select x #b01) (select y #b01)))
         (z2 (bvshl (select x #b10) (select y #b10)))
         (z3 (bvshl (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvshl_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvshl (select x #b00) (select y #b00)))
         (z1 (bvshl (select x #b01) (select y #b01)))
         (z2 (bvshl (select x #b10) (select y #b10)))
         (z3 (bvshl (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvsdiv_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvsdiv (select x #b00) (select y #b00)))
         (z1 (bvsdiv (select x #b01) (select y #b01)))
         (z2 (bvsdiv (select x #b10) (select y #b10)))
         (z3 (bvsdiv (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvsdiv_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvsdiv (select x #b00) (select y #b00)))
         (z1 (bvsdiv (select x #b01) (select y #b01)))
         (z2 (bvsdiv (select x #b10) (select y #b10)))
         (z3 (bvsdiv (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvsdiv_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvsdiv (select x #b00) (select y #b00)))
         (z1 (bvsdiv (select x #b01) (select y #b01)))
         (z2 (bvsdiv (select x #b10) (select y #b10)))
         (z3 (bvsdiv (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvsdiv_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvsdiv (select x #b00) (select y #b00)))
         (z1 (bvsdiv (select x #b01) (select y #b01)))
         (z2 (bvsdiv (select x #b10) (select y #b10)))
         (z3 (bvsdiv (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvudiv_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvudiv (select x #b00) (select y #b00)))
         (z1 (bvudiv (select x #b01) (select y #b01)))
         (z2 (bvudiv (select x #b10) (select y #b10)))
         (z3 (bvudiv (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvudiv_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvudiv (select x #b00) (select y #b00)))
         (z1 (bvudiv (select x #b01) (select y #b01)))
         (z2 (bvudiv (select x #b10) (select y #b10)))
         (z3 (bvudiv (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvudiv_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvudiv (select x #b00) (select y #b00)))
         (z1 (bvudiv (select x #b01) (select y #b01)))
         (z2 (bvudiv (select x #b10) (select y #b10)))
         (z3 (bvudiv (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvudiv_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvudiv (select x #b00) (select y #b00)))
         (z1 (bvudiv (select x #b01) (select y #b01)))
         (z2 (bvudiv (select x #b10) (select y #b10)))
         (z3 (bvudiv (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvlshr_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvlshr (select x #b00) (select y #b00)))
         (z1 (bvlshr (select x #b01) (select y #b01)))
         (z2 (bvlshr (select x #b10) (select y #b10)))
         (z3 (bvlshr (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvlshr_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvlshr (select x #b00) (select y #b00)))
         (z1 (bvlshr (select x #b01) (select y #b01)))
         (z2 (bvlshr (select x #b10) (select y #b10)))
         (z3 (bvlshr (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvlshr_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvlshr (select x #b00) (select y #b00)))
         (z1 (bvlshr (select x #b01) (select y #b01)))
         (z2 (bvlshr (select x #b10) (select y #b10)))
         (z3 (bvlshr (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvlshr_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvlshr (select x #b00) (select y #b00)))
         (z1 (bvlshr (select x #b01) (select y #b01)))
         (z2 (bvlshr (select x #b10) (select y #b10)))
         (z3 (bvlshr (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvashr_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvashr (select x #b00) (select y #b00)))
         (z1 (bvashr (select x #b01) (select y #b01)))
         (z2 (bvashr (select x #b10) (select y #b10)))
         (z3 (bvashr (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvashr_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvashr (select x #b00) (select y #b00)))
         (z1 (bvashr (select x #b01) (select y #b01)))
         (z2 (bvashr (select x #b10) (select y #b10)))
         (z3 (bvashr (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvashr_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvashr (select x #b00) (select y #b00)))
         (z1 (bvashr (select x #b01) (select y #b01)))
         (z2 (bvashr (select x #b10) (select y #b10)))
         (z3 (bvashr (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvashr_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvashr (select x #b00) (select y #b00)))
         (z1 (bvashr (select x #b01) (select y #b01)))
         (z2 (bvashr (select x #b10) (select y #b10)))
         (z3 (bvashr (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvurem_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvurem (select x #b00) (select y #b00)))
         (z1 (bvurem (select x #b01) (select y #b01)))
         (z2 (bvurem (select x #b10) (select y #b10)))
         (z3 (bvurem (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvurem_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvurem (select x #b00) (select y #b00)))
         (z1 (bvurem (select x #b01) (select y #b01)))
         (z2 (bvurem (select x #b10) (select y #b10)))
         (z3 (bvurem (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvurem_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvurem (select x #b00) (select y #b00)))
         (z1 (bvurem (select x #b01) (select y #b01)))
         (z2 (bvurem (select x #b10) (select y #b10)))
         (z3 (bvurem (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvurem_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvurem (select x #b00) (select y #b00)))
         (z1 (bvurem (select x #b01) (select y #b01)))
         (z2 (bvurem (select x #b10) (select y #b10)))
         (z3 (bvurem (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvsrem_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvsrem (select x #b00) (select y #b00)))
         (z1 (bvsrem (select x #b01) (select y #b01)))
         (z2 (bvsrem (select x #b10) (select y #b10)))
         (z3 (bvsrem (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvsrem_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvsrem (select x #b00) (select y #b00)))
         (z1 (bvsrem (select x #b01) (select y #b01)))
         (z2 (bvsrem (select x #b10) (select y #b10)))
         (z3 (bvsrem (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvsrem_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvsrem (select x #b00) (select y #b00)))
         (z1 (bvsrem (select x #b01) (select y #b01)))
         (z2 (bvsrem (select x #b10) (select y #b10)))
         (z3 (bvsrem (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvsrem_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvsrem (select x #b00) (select y #b00)))
         (z1 (bvsrem (select x #b01) (select y #b01)))
         (z2 (bvsrem (select x #b10) (select y #b10)))
         (z3 (bvsrem (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvand_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvand (select x #b00) (select y #b00)))
         (z1 (bvand (select x #b01) (select y #b01)))
         (z2 (bvand (select x #b10) (select y #b10)))
         (z3 (bvand (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvand_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvand (select x #b00) (select y #b00)))
         (z1 (bvand (select x #b01) (select y #b01)))
         (z2 (bvand (select x #b10) (select y #b10)))
         (z3 (bvand (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvand_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvand (select x #b00) (select y #b00)))
         (z1 (bvand (select x #b01) (select y #b01)))
         (z2 (bvand (select x #b10) (select y #b10)))
         (z3 (bvand (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvand_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvand (select x #b00) (select y #b00)))
         (z1 (bvand (select x #b01) (select y #b01)))
         (z2 (bvand (select x #b10) (select y #b10)))
         (z3 (bvand (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvor_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvor (select x #b00) (select y #b00)))
         (z1 (bvor (select x #b01) (select y #b01)))
         (z2 (bvor (select x #b10) (select y #b10)))
         (z3 (bvor (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvor_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvor (select x #b00) (select y #b00)))
         (z1 (bvor (select x #b01) (select y #b01)))
         (z2 (bvor (select x #b10) (select y #b10)))
         (z3 (bvor (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvor_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvor (select x #b00) (select y #b00)))
         (z1 (bvor (select x #b01) (select y #b01)))
         (z2 (bvor (select x #b10) (select y #b10)))
         (z3 (bvor (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvor_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvor (select x #b00) (select y #b00)))
         (z1 (bvor (select x #b01) (select y #b01)))
         (z2 (bvor (select x #b10) (select y #b10)))
         (z3 (bvor (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvxor_2_8 ((x vector_2_8) (y vector_2_8)) vector_2_8
   (let ((z0 (bvxor (select x #b00) (select y #b00)))
         (z1 (bvxor (select x #b01) (select y #b01)))
         (z2 (bvxor (select x #b10) (select y #b10)))
         (z3 (bvxor (select x #b11) (select y #b11))))
      (vmake_2_8 z0 z1 z2 z3)))


 (define-fun vbvxor_2_16 ((x vector_2_16) (y vector_2_16)) vector_2_16
   (let ((z0 (bvxor (select x #b00) (select y #b00)))
         (z1 (bvxor (select x #b01) (select y #b01)))
         (z2 (bvxor (select x #b10) (select y #b10)))
         (z3 (bvxor (select x #b11) (select y #b11))))
      (vmake_2_16 z0 z1 z2 z3)))


 (define-fun vbvxor_2_32 ((x vector_2_32) (y vector_2_32)) vector_2_32
   (let ((z0 (bvxor (select x #b00) (select y #b00)))
         (z1 (bvxor (select x #b01) (select y #b01)))
         (z2 (bvxor (select x #b10) (select y #b10)))
         (z3 (bvxor (select x #b11) (select y #b11))))
      (vmake_2_32 z0 z1 z2 z3)))


 (define-fun vbvxor_2_64 ((x vector_2_64) (y vector_2_64)) vector_2_64
   (let ((z0 (bvxor (select x #b00) (select y #b00)))
         (z1 (bvxor (select x #b01) (select y #b01)))
         (z2 (bvxor (select x #b10) (select y #b10)))
         (z3 (bvxor (select x #b11) (select y #b11))))
      (vmake_2_64 z0 z1 z2 z3)))


 (define-fun vbvadd_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvadd (select x #b000) (select y #b000)))
         (z1 (bvadd (select x #b001) (select y #b001)))
         (z2 (bvadd (select x #b010) (select y #b010)))
         (z3 (bvadd (select x #b011) (select y #b011)))
         (z4 (bvadd (select x #b100) (select y #b100)))
         (z5 (bvadd (select x #b101) (select y #b101)))
         (z6 (bvadd (select x #b110) (select y #b110)))
         (z7 (bvadd (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvadd_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvadd (select x #b000) (select y #b000)))
         (z1 (bvadd (select x #b001) (select y #b001)))
         (z2 (bvadd (select x #b010) (select y #b010)))
         (z3 (bvadd (select x #b011) (select y #b011)))
         (z4 (bvadd (select x #b100) (select y #b100)))
         (z5 (bvadd (select x #b101) (select y #b101)))
         (z6 (bvadd (select x #b110) (select y #b110)))
         (z7 (bvadd (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvadd_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvadd (select x #b000) (select y #b000)))
         (z1 (bvadd (select x #b001) (select y #b001)))
         (z2 (bvadd (select x #b010) (select y #b010)))
         (z3 (bvadd (select x #b011) (select y #b011)))
         (z4 (bvadd (select x #b100) (select y #b100)))
         (z5 (bvadd (select x #b101) (select y #b101)))
         (z6 (bvadd (select x #b110) (select y #b110)))
         (z7 (bvadd (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvadd_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvadd (select x #b000) (select y #b000)))
         (z1 (bvadd (select x #b001) (select y #b001)))
         (z2 (bvadd (select x #b010) (select y #b010)))
         (z3 (bvadd (select x #b011) (select y #b011)))
         (z4 (bvadd (select x #b100) (select y #b100)))
         (z5 (bvadd (select x #b101) (select y #b101)))
         (z6 (bvadd (select x #b110) (select y #b110)))
         (z7 (bvadd (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsub_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvsub (select x #b000) (select y #b000)))
         (z1 (bvsub (select x #b001) (select y #b001)))
         (z2 (bvsub (select x #b010) (select y #b010)))
         (z3 (bvsub (select x #b011) (select y #b011)))
         (z4 (bvsub (select x #b100) (select y #b100)))
         (z5 (bvsub (select x #b101) (select y #b101)))
         (z6 (bvsub (select x #b110) (select y #b110)))
         (z7 (bvsub (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsub_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvsub (select x #b000) (select y #b000)))
         (z1 (bvsub (select x #b001) (select y #b001)))
         (z2 (bvsub (select x #b010) (select y #b010)))
         (z3 (bvsub (select x #b011) (select y #b011)))
         (z4 (bvsub (select x #b100) (select y #b100)))
         (z5 (bvsub (select x #b101) (select y #b101)))
         (z6 (bvsub (select x #b110) (select y #b110)))
         (z7 (bvsub (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsub_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvsub (select x #b000) (select y #b000)))
         (z1 (bvsub (select x #b001) (select y #b001)))
         (z2 (bvsub (select x #b010) (select y #b010)))
         (z3 (bvsub (select x #b011) (select y #b011)))
         (z4 (bvsub (select x #b100) (select y #b100)))
         (z5 (bvsub (select x #b101) (select y #b101)))
         (z6 (bvsub (select x #b110) (select y #b110)))
         (z7 (bvsub (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsub_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvsub (select x #b000) (select y #b000)))
         (z1 (bvsub (select x #b001) (select y #b001)))
         (z2 (bvsub (select x #b010) (select y #b010)))
         (z3 (bvsub (select x #b011) (select y #b011)))
         (z4 (bvsub (select x #b100) (select y #b100)))
         (z5 (bvsub (select x #b101) (select y #b101)))
         (z6 (bvsub (select x #b110) (select y #b110)))
         (z7 (bvsub (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvmul_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvmul (select x #b000) (select y #b000)))
         (z1 (bvmul (select x #b001) (select y #b001)))
         (z2 (bvmul (select x #b010) (select y #b010)))
         (z3 (bvmul (select x #b011) (select y #b011)))
         (z4 (bvmul (select x #b100) (select y #b100)))
         (z5 (bvmul (select x #b101) (select y #b101)))
         (z6 (bvmul (select x #b110) (select y #b110)))
         (z7 (bvmul (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvmul_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvmul (select x #b000) (select y #b000)))
         (z1 (bvmul (select x #b001) (select y #b001)))
         (z2 (bvmul (select x #b010) (select y #b010)))
         (z3 (bvmul (select x #b011) (select y #b011)))
         (z4 (bvmul (select x #b100) (select y #b100)))
         (z5 (bvmul (select x #b101) (select y #b101)))
         (z6 (bvmul (select x #b110) (select y #b110)))
         (z7 (bvmul (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvmul_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvmul (select x #b000) (select y #b000)))
         (z1 (bvmul (select x #b001) (select y #b001)))
         (z2 (bvmul (select x #b010) (select y #b010)))
         (z3 (bvmul (select x #b011) (select y #b011)))
         (z4 (bvmul (select x #b100) (select y #b100)))
         (z5 (bvmul (select x #b101) (select y #b101)))
         (z6 (bvmul (select x #b110) (select y #b110)))
         (z7 (bvmul (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvmul_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvmul (select x #b000) (select y #b000)))
         (z1 (bvmul (select x #b001) (select y #b001)))
         (z2 (bvmul (select x #b010) (select y #b010)))
         (z3 (bvmul (select x #b011) (select y #b011)))
         (z4 (bvmul (select x #b100) (select y #b100)))
         (z5 (bvmul (select x #b101) (select y #b101)))
         (z6 (bvmul (select x #b110) (select y #b110)))
         (z7 (bvmul (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvshl_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvshl (select x #b000) (select y #b000)))
         (z1 (bvshl (select x #b001) (select y #b001)))
         (z2 (bvshl (select x #b010) (select y #b010)))
         (z3 (bvshl (select x #b011) (select y #b011)))
         (z4 (bvshl (select x #b100) (select y #b100)))
         (z5 (bvshl (select x #b101) (select y #b101)))
         (z6 (bvshl (select x #b110) (select y #b110)))
         (z7 (bvshl (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvshl_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvshl (select x #b000) (select y #b000)))
         (z1 (bvshl (select x #b001) (select y #b001)))
         (z2 (bvshl (select x #b010) (select y #b010)))
         (z3 (bvshl (select x #b011) (select y #b011)))
         (z4 (bvshl (select x #b100) (select y #b100)))
         (z5 (bvshl (select x #b101) (select y #b101)))
         (z6 (bvshl (select x #b110) (select y #b110)))
         (z7 (bvshl (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvshl_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvshl (select x #b000) (select y #b000)))
         (z1 (bvshl (select x #b001) (select y #b001)))
         (z2 (bvshl (select x #b010) (select y #b010)))
         (z3 (bvshl (select x #b011) (select y #b011)))
         (z4 (bvshl (select x #b100) (select y #b100)))
         (z5 (bvshl (select x #b101) (select y #b101)))
         (z6 (bvshl (select x #b110) (select y #b110)))
         (z7 (bvshl (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvshl_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvshl (select x #b000) (select y #b000)))
         (z1 (bvshl (select x #b001) (select y #b001)))
         (z2 (bvshl (select x #b010) (select y #b010)))
         (z3 (bvshl (select x #b011) (select y #b011)))
         (z4 (bvshl (select x #b100) (select y #b100)))
         (z5 (bvshl (select x #b101) (select y #b101)))
         (z6 (bvshl (select x #b110) (select y #b110)))
         (z7 (bvshl (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsdiv_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvsdiv (select x #b000) (select y #b000)))
         (z1 (bvsdiv (select x #b001) (select y #b001)))
         (z2 (bvsdiv (select x #b010) (select y #b010)))
         (z3 (bvsdiv (select x #b011) (select y #b011)))
         (z4 (bvsdiv (select x #b100) (select y #b100)))
         (z5 (bvsdiv (select x #b101) (select y #b101)))
         (z6 (bvsdiv (select x #b110) (select y #b110)))
         (z7 (bvsdiv (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsdiv_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvsdiv (select x #b000) (select y #b000)))
         (z1 (bvsdiv (select x #b001) (select y #b001)))
         (z2 (bvsdiv (select x #b010) (select y #b010)))
         (z3 (bvsdiv (select x #b011) (select y #b011)))
         (z4 (bvsdiv (select x #b100) (select y #b100)))
         (z5 (bvsdiv (select x #b101) (select y #b101)))
         (z6 (bvsdiv (select x #b110) (select y #b110)))
         (z7 (bvsdiv (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsdiv_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvsdiv (select x #b000) (select y #b000)))
         (z1 (bvsdiv (select x #b001) (select y #b001)))
         (z2 (bvsdiv (select x #b010) (select y #b010)))
         (z3 (bvsdiv (select x #b011) (select y #b011)))
         (z4 (bvsdiv (select x #b100) (select y #b100)))
         (z5 (bvsdiv (select x #b101) (select y #b101)))
         (z6 (bvsdiv (select x #b110) (select y #b110)))
         (z7 (bvsdiv (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsdiv_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvsdiv (select x #b000) (select y #b000)))
         (z1 (bvsdiv (select x #b001) (select y #b001)))
         (z2 (bvsdiv (select x #b010) (select y #b010)))
         (z3 (bvsdiv (select x #b011) (select y #b011)))
         (z4 (bvsdiv (select x #b100) (select y #b100)))
         (z5 (bvsdiv (select x #b101) (select y #b101)))
         (z6 (bvsdiv (select x #b110) (select y #b110)))
         (z7 (bvsdiv (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvudiv_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvudiv (select x #b000) (select y #b000)))
         (z1 (bvudiv (select x #b001) (select y #b001)))
         (z2 (bvudiv (select x #b010) (select y #b010)))
         (z3 (bvudiv (select x #b011) (select y #b011)))
         (z4 (bvudiv (select x #b100) (select y #b100)))
         (z5 (bvudiv (select x #b101) (select y #b101)))
         (z6 (bvudiv (select x #b110) (select y #b110)))
         (z7 (bvudiv (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvudiv_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvudiv (select x #b000) (select y #b000)))
         (z1 (bvudiv (select x #b001) (select y #b001)))
         (z2 (bvudiv (select x #b010) (select y #b010)))
         (z3 (bvudiv (select x #b011) (select y #b011)))
         (z4 (bvudiv (select x #b100) (select y #b100)))
         (z5 (bvudiv (select x #b101) (select y #b101)))
         (z6 (bvudiv (select x #b110) (select y #b110)))
         (z7 (bvudiv (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvudiv_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvudiv (select x #b000) (select y #b000)))
         (z1 (bvudiv (select x #b001) (select y #b001)))
         (z2 (bvudiv (select x #b010) (select y #b010)))
         (z3 (bvudiv (select x #b011) (select y #b011)))
         (z4 (bvudiv (select x #b100) (select y #b100)))
         (z5 (bvudiv (select x #b101) (select y #b101)))
         (z6 (bvudiv (select x #b110) (select y #b110)))
         (z7 (bvudiv (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvudiv_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvudiv (select x #b000) (select y #b000)))
         (z1 (bvudiv (select x #b001) (select y #b001)))
         (z2 (bvudiv (select x #b010) (select y #b010)))
         (z3 (bvudiv (select x #b011) (select y #b011)))
         (z4 (bvudiv (select x #b100) (select y #b100)))
         (z5 (bvudiv (select x #b101) (select y #b101)))
         (z6 (bvudiv (select x #b110) (select y #b110)))
         (z7 (bvudiv (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvlshr_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvlshr (select x #b000) (select y #b000)))
         (z1 (bvlshr (select x #b001) (select y #b001)))
         (z2 (bvlshr (select x #b010) (select y #b010)))
         (z3 (bvlshr (select x #b011) (select y #b011)))
         (z4 (bvlshr (select x #b100) (select y #b100)))
         (z5 (bvlshr (select x #b101) (select y #b101)))
         (z6 (bvlshr (select x #b110) (select y #b110)))
         (z7 (bvlshr (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvlshr_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvlshr (select x #b000) (select y #b000)))
         (z1 (bvlshr (select x #b001) (select y #b001)))
         (z2 (bvlshr (select x #b010) (select y #b010)))
         (z3 (bvlshr (select x #b011) (select y #b011)))
         (z4 (bvlshr (select x #b100) (select y #b100)))
         (z5 (bvlshr (select x #b101) (select y #b101)))
         (z6 (bvlshr (select x #b110) (select y #b110)))
         (z7 (bvlshr (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvlshr_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvlshr (select x #b000) (select y #b000)))
         (z1 (bvlshr (select x #b001) (select y #b001)))
         (z2 (bvlshr (select x #b010) (select y #b010)))
         (z3 (bvlshr (select x #b011) (select y #b011)))
         (z4 (bvlshr (select x #b100) (select y #b100)))
         (z5 (bvlshr (select x #b101) (select y #b101)))
         (z6 (bvlshr (select x #b110) (select y #b110)))
         (z7 (bvlshr (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvlshr_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvlshr (select x #b000) (select y #b000)))
         (z1 (bvlshr (select x #b001) (select y #b001)))
         (z2 (bvlshr (select x #b010) (select y #b010)))
         (z3 (bvlshr (select x #b011) (select y #b011)))
         (z4 (bvlshr (select x #b100) (select y #b100)))
         (z5 (bvlshr (select x #b101) (select y #b101)))
         (z6 (bvlshr (select x #b110) (select y #b110)))
         (z7 (bvlshr (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvashr_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvashr (select x #b000) (select y #b000)))
         (z1 (bvashr (select x #b001) (select y #b001)))
         (z2 (bvashr (select x #b010) (select y #b010)))
         (z3 (bvashr (select x #b011) (select y #b011)))
         (z4 (bvashr (select x #b100) (select y #b100)))
         (z5 (bvashr (select x #b101) (select y #b101)))
         (z6 (bvashr (select x #b110) (select y #b110)))
         (z7 (bvashr (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvashr_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvashr (select x #b000) (select y #b000)))
         (z1 (bvashr (select x #b001) (select y #b001)))
         (z2 (bvashr (select x #b010) (select y #b010)))
         (z3 (bvashr (select x #b011) (select y #b011)))
         (z4 (bvashr (select x #b100) (select y #b100)))
         (z5 (bvashr (select x #b101) (select y #b101)))
         (z6 (bvashr (select x #b110) (select y #b110)))
         (z7 (bvashr (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvashr_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvashr (select x #b000) (select y #b000)))
         (z1 (bvashr (select x #b001) (select y #b001)))
         (z2 (bvashr (select x #b010) (select y #b010)))
         (z3 (bvashr (select x #b011) (select y #b011)))
         (z4 (bvashr (select x #b100) (select y #b100)))
         (z5 (bvashr (select x #b101) (select y #b101)))
         (z6 (bvashr (select x #b110) (select y #b110)))
         (z7 (bvashr (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvashr_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvashr (select x #b000) (select y #b000)))
         (z1 (bvashr (select x #b001) (select y #b001)))
         (z2 (bvashr (select x #b010) (select y #b010)))
         (z3 (bvashr (select x #b011) (select y #b011)))
         (z4 (bvashr (select x #b100) (select y #b100)))
         (z5 (bvashr (select x #b101) (select y #b101)))
         (z6 (bvashr (select x #b110) (select y #b110)))
         (z7 (bvashr (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvurem_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvurem (select x #b000) (select y #b000)))
         (z1 (bvurem (select x #b001) (select y #b001)))
         (z2 (bvurem (select x #b010) (select y #b010)))
         (z3 (bvurem (select x #b011) (select y #b011)))
         (z4 (bvurem (select x #b100) (select y #b100)))
         (z5 (bvurem (select x #b101) (select y #b101)))
         (z6 (bvurem (select x #b110) (select y #b110)))
         (z7 (bvurem (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvurem_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvurem (select x #b000) (select y #b000)))
         (z1 (bvurem (select x #b001) (select y #b001)))
         (z2 (bvurem (select x #b010) (select y #b010)))
         (z3 (bvurem (select x #b011) (select y #b011)))
         (z4 (bvurem (select x #b100) (select y #b100)))
         (z5 (bvurem (select x #b101) (select y #b101)))
         (z6 (bvurem (select x #b110) (select y #b110)))
         (z7 (bvurem (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvurem_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvurem (select x #b000) (select y #b000)))
         (z1 (bvurem (select x #b001) (select y #b001)))
         (z2 (bvurem (select x #b010) (select y #b010)))
         (z3 (bvurem (select x #b011) (select y #b011)))
         (z4 (bvurem (select x #b100) (select y #b100)))
         (z5 (bvurem (select x #b101) (select y #b101)))
         (z6 (bvurem (select x #b110) (select y #b110)))
         (z7 (bvurem (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvurem_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvurem (select x #b000) (select y #b000)))
         (z1 (bvurem (select x #b001) (select y #b001)))
         (z2 (bvurem (select x #b010) (select y #b010)))
         (z3 (bvurem (select x #b011) (select y #b011)))
         (z4 (bvurem (select x #b100) (select y #b100)))
         (z5 (bvurem (select x #b101) (select y #b101)))
         (z6 (bvurem (select x #b110) (select y #b110)))
         (z7 (bvurem (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsrem_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvsrem (select x #b000) (select y #b000)))
         (z1 (bvsrem (select x #b001) (select y #b001)))
         (z2 (bvsrem (select x #b010) (select y #b010)))
         (z3 (bvsrem (select x #b011) (select y #b011)))
         (z4 (bvsrem (select x #b100) (select y #b100)))
         (z5 (bvsrem (select x #b101) (select y #b101)))
         (z6 (bvsrem (select x #b110) (select y #b110)))
         (z7 (bvsrem (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsrem_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvsrem (select x #b000) (select y #b000)))
         (z1 (bvsrem (select x #b001) (select y #b001)))
         (z2 (bvsrem (select x #b010) (select y #b010)))
         (z3 (bvsrem (select x #b011) (select y #b011)))
         (z4 (bvsrem (select x #b100) (select y #b100)))
         (z5 (bvsrem (select x #b101) (select y #b101)))
         (z6 (bvsrem (select x #b110) (select y #b110)))
         (z7 (bvsrem (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsrem_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvsrem (select x #b000) (select y #b000)))
         (z1 (bvsrem (select x #b001) (select y #b001)))
         (z2 (bvsrem (select x #b010) (select y #b010)))
         (z3 (bvsrem (select x #b011) (select y #b011)))
         (z4 (bvsrem (select x #b100) (select y #b100)))
         (z5 (bvsrem (select x #b101) (select y #b101)))
         (z6 (bvsrem (select x #b110) (select y #b110)))
         (z7 (bvsrem (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvsrem_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvsrem (select x #b000) (select y #b000)))
         (z1 (bvsrem (select x #b001) (select y #b001)))
         (z2 (bvsrem (select x #b010) (select y #b010)))
         (z3 (bvsrem (select x #b011) (select y #b011)))
         (z4 (bvsrem (select x #b100) (select y #b100)))
         (z5 (bvsrem (select x #b101) (select y #b101)))
         (z6 (bvsrem (select x #b110) (select y #b110)))
         (z7 (bvsrem (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvand_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvand (select x #b000) (select y #b000)))
         (z1 (bvand (select x #b001) (select y #b001)))
         (z2 (bvand (select x #b010) (select y #b010)))
         (z3 (bvand (select x #b011) (select y #b011)))
         (z4 (bvand (select x #b100) (select y #b100)))
         (z5 (bvand (select x #b101) (select y #b101)))
         (z6 (bvand (select x #b110) (select y #b110)))
         (z7 (bvand (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvand_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvand (select x #b000) (select y #b000)))
         (z1 (bvand (select x #b001) (select y #b001)))
         (z2 (bvand (select x #b010) (select y #b010)))
         (z3 (bvand (select x #b011) (select y #b011)))
         (z4 (bvand (select x #b100) (select y #b100)))
         (z5 (bvand (select x #b101) (select y #b101)))
         (z6 (bvand (select x #b110) (select y #b110)))
         (z7 (bvand (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvand_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvand (select x #b000) (select y #b000)))
         (z1 (bvand (select x #b001) (select y #b001)))
         (z2 (bvand (select x #b010) (select y #b010)))
         (z3 (bvand (select x #b011) (select y #b011)))
         (z4 (bvand (select x #b100) (select y #b100)))
         (z5 (bvand (select x #b101) (select y #b101)))
         (z6 (bvand (select x #b110) (select y #b110)))
         (z7 (bvand (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvand_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvand (select x #b000) (select y #b000)))
         (z1 (bvand (select x #b001) (select y #b001)))
         (z2 (bvand (select x #b010) (select y #b010)))
         (z3 (bvand (select x #b011) (select y #b011)))
         (z4 (bvand (select x #b100) (select y #b100)))
         (z5 (bvand (select x #b101) (select y #b101)))
         (z6 (bvand (select x #b110) (select y #b110)))
         (z7 (bvand (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvor_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvor (select x #b000) (select y #b000)))
         (z1 (bvor (select x #b001) (select y #b001)))
         (z2 (bvor (select x #b010) (select y #b010)))
         (z3 (bvor (select x #b011) (select y #b011)))
         (z4 (bvor (select x #b100) (select y #b100)))
         (z5 (bvor (select x #b101) (select y #b101)))
         (z6 (bvor (select x #b110) (select y #b110)))
         (z7 (bvor (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvor_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvor (select x #b000) (select y #b000)))
         (z1 (bvor (select x #b001) (select y #b001)))
         (z2 (bvor (select x #b010) (select y #b010)))
         (z3 (bvor (select x #b011) (select y #b011)))
         (z4 (bvor (select x #b100) (select y #b100)))
         (z5 (bvor (select x #b101) (select y #b101)))
         (z6 (bvor (select x #b110) (select y #b110)))
         (z7 (bvor (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvor_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvor (select x #b000) (select y #b000)))
         (z1 (bvor (select x #b001) (select y #b001)))
         (z2 (bvor (select x #b010) (select y #b010)))
         (z3 (bvor (select x #b011) (select y #b011)))
         (z4 (bvor (select x #b100) (select y #b100)))
         (z5 (bvor (select x #b101) (select y #b101)))
         (z6 (bvor (select x #b110) (select y #b110)))
         (z7 (bvor (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvor_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvor (select x #b000) (select y #b000)))
         (z1 (bvor (select x #b001) (select y #b001)))
         (z2 (bvor (select x #b010) (select y #b010)))
         (z3 (bvor (select x #b011) (select y #b011)))
         (z4 (bvor (select x #b100) (select y #b100)))
         (z5 (bvor (select x #b101) (select y #b101)))
         (z6 (bvor (select x #b110) (select y #b110)))
         (z7 (bvor (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvxor_3_8 ((x vector_3_8) (y vector_3_8)) vector_3_8
   (let (
         (z0 (bvxor (select x #b000) (select y #b000)))
         (z1 (bvxor (select x #b001) (select y #b001)))
         (z2 (bvxor (select x #b010) (select y #b010)))
         (z3 (bvxor (select x #b011) (select y #b011)))
         (z4 (bvxor (select x #b100) (select y #b100)))
         (z5 (bvxor (select x #b101) (select y #b101)))
         (z6 (bvxor (select x #b110) (select y #b110)))
         (z7 (bvxor (select x #b111) (select y #b111)))
         )
      (vmake_3_8 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvxor_3_16 ((x vector_3_16) (y vector_3_16)) vector_3_16
   (let (
         (z0 (bvxor (select x #b000) (select y #b000)))
         (z1 (bvxor (select x #b001) (select y #b001)))
         (z2 (bvxor (select x #b010) (select y #b010)))
         (z3 (bvxor (select x #b011) (select y #b011)))
         (z4 (bvxor (select x #b100) (select y #b100)))
         (z5 (bvxor (select x #b101) (select y #b101)))
         (z6 (bvxor (select x #b110) (select y #b110)))
         (z7 (bvxor (select x #b111) (select y #b111)))
         )
      (vmake_3_16 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvxor_3_32 ((x vector_3_32) (y vector_3_32)) vector_3_32
   (let (
         (z0 (bvxor (select x #b000) (select y #b000)))
         (z1 (bvxor (select x #b001) (select y #b001)))
         (z2 (bvxor (select x #b010) (select y #b010)))
         (z3 (bvxor (select x #b011) (select y #b011)))
         (z4 (bvxor (select x #b100) (select y #b100)))
         (z5 (bvxor (select x #b101) (select y #b101)))
         (z6 (bvxor (select x #b110) (select y #b110)))
         (z7 (bvxor (select x #b111) (select y #b111)))
         )
      (vmake_3_32 z0 z1 z2 z3 z4 z5 z6 z7)))


 (define-fun vbvxor_3_64 ((x vector_3_64) (y vector_3_64)) vector_3_64
   (let (
         (z0 (bvxor (select x #b000) (select y #b000)))
         (z1 (bvxor (select x #b001) (select y #b001)))
         (z2 (bvxor (select x #b010) (select y #b010)))
         (z3 (bvxor (select x #b011) (select y #b011)))
         (z4 (bvxor (select x #b100) (select y #b100)))
         (z5 (bvxor (select x #b101) (select y #b101)))
         (z6 (bvxor (select x #b110) (select y #b110)))
         (z7 (bvxor (select x #b111) (select y #b111)))
         )
      (vmake_3_64 z0 z1 z2 z3 z4 z5 z6 z7)))




;; conversion to and from (Bitvector 64)
;; this assumes little endian representation
(define-fun cast_vector_1_32_to_bits ((x vector_1_32)) (_ BitVec 64)
   (concat (select x #b1) (select x #b0)))

(define-fun cast_bits_to_vector_1_32 ((w (_ BitVec 64))) vector_1_32
   (let ((z0 ((_ extract 31 0) w))
         (z1 ((_ extract 63 32) w)))
      (vmake_1_32 z0 z1)))

;; conversion to and from (Bitvector 128)
;; this assumes little endian representation
(define-fun cast_vector_2_32_to_bits ((x vector_2_32)) (_ BitVec 128)
   (concat (concat (concat (select x #b11) (select x #b10)) (select x #b01)) (select x #b00)))

(define-fun cast_bits_to_vector_2_32 ((w (_ BitVec 128))) vector_2_32
   (let ((z0 ((_ extract 31 0) w))
         (z1 ((_ extract 63 32) w))
         (z2 ((_ extract 95 64) w))
         (z3 ((_ extract 127 96) w)))
      (vmake_2_32 z0 z1 z2 z3)))

;; conversion to and from (Bitvector 128) to vectors 2 x i64
(define-fun cast_vector_1_64_to_bits ((x vector_1_64)) (_ BitVec 128)
   (concat (select x #b1) (select x #b0)))

(define-fun cast_bits_to_vector_1_64 ((w (_ BitVec 128))) vector_1_64
   (let ((z0 ((_ extract 63 0) w))
         (z1 ((_ extract 127 64) w)))
      (vmake_1_64 z0 z1)))




;; end of prelude


;; @.str [16 x i8] = c"lhs == rhs: %d\0A\00"
(declare-fun |-@.str| () (_ BitVec 64))
;; @.str1 [22 x i8] = c"lhs = %d != rhs = %d\0A\00"
(declare-fun |-@.str1| () (_ BitVec 64))
(declare-fun -@lhs () (_ BitVec 64))
(declare-fun -@rhs () (_ BitVec 64))
(declare-fun -@main () (_ BitVec 64))
(declare-fun -@atoi () (_ BitVec 64))
(declare-fun -@printf () (_ BitVec 64))


;; Function: |-@lhs|
;; (i32 %a, i32 %b)
(declare-fun memory1 () Mem)
(define-fun rsp1 () (_ BitVec 64) (_ bv0 64))
(declare-fun |%a-@lhs| () (_ BitVec 32))
(declare-fun |%b-@lhs| () (_ BitVec 32))

;; BLOCK %0 with index 0 and rank = 1
;; Predecessors:
;; |-@lhs_block_0_entry_condition| 
(define-fun |-@lhs_block_0_entry_condition| () Bool true)
;; %1 = insertelement <2 x i32> undef, i32 %a, i32 0
(define-fun |%1-@lhs| () (Array (_ BitVec 1) (_ BitVec 32)) (store vzero_1_32 ((_ extract 0 0) (_ bv0 32)) |%a-@lhs|))
;; %2 = insertelement <2 x i32> %1, i32 %b, i32 1
(define-fun |%2-@lhs| () (Array (_ BitVec 1) (_ BitVec 32)) (store |%1-@lhs| ((_ extract 0 0) (_ bv1 32)) |%b-@lhs|))
;; %3 = shufflevector <2 x i32> %2, <2 x i32> undef, <2 x i32> <i32 1, i32 0>
(define-fun |%3-@lhs| () (Array (_ BitVec 1) (_ BitVec 32)) 
(let ((x0 vundef_1_32))
(let ((x1 (store x0 ((_ extract 0 0) (_ bv0 32)) (select |%2-@lhs| ((_ extract 0 0) (_ bv1 32))))))
 (let ((x2 (store x1 ((_ extract 0 0) (_ bv1 32)) (select |%2-@lhs| ((_ extract 0 0) (_ bv0 32))))))
  x2))))
;; %4 = shufflevector <2 x i32> %3, <2 x i32> undef, <2 x i32> <i32 1, i32 0>
(define-fun |%4-@lhs| () (Array (_ BitVec 1) (_ BitVec 32)) 
(let ((x0 vundef_1_32))
(let ((x1 (store x0 ((_ extract 0 0) (_ bv0 32)) (select |%3-@lhs| ((_ extract 0 0) (_ bv1 32))))))
 (let ((x2 (store x1 ((_ extract 0 0) (_ bv1 32)) (select |%3-@lhs| ((_ extract 0 0) (_ bv0 32))))))
  x2))))
;; %5 = extractelement <2 x i32> %4, i32 0
(define-fun |%5-@lhs| () (_ BitVec 32) (select |%4-@lhs| ((_ extract 0 0) (_ bv0 32))))
;; ret i32 %5
;; No backward arrows


(define-fun |-@lhs_result| () (_ BitVec 32) |%5-@lhs|)

;; Function: |-@rhs|
;; (i32 %a, i32 %b)
(declare-fun memory2 () Mem)
(define-fun rsp2 () (_ BitVec 64) (_ bv0 64))
(declare-fun |%a-@rhs| () (_ BitVec 32))
(declare-fun |%b-@rhs| () (_ BitVec 32))

;; BLOCK %0 with index 0 and rank = 1
;; Predecessors:
;; |-@rhs_block_0_entry_condition| 
(define-fun |-@rhs_block_0_entry_condition| () Bool true)
;; %1 = insertelement <2 x i32> undef, i32 %a, i32 0
(define-fun |%1-@rhs| () (Array (_ BitVec 1) (_ BitVec 32)) (store vzero_1_32 ((_ extract 0 0) (_ bv0 32)) |%a-@rhs|))
;; %2 = insertelement <2 x i32> %1, i32 %b, i32 1
(define-fun |%2-@rhs| () (Array (_ BitVec 1) (_ BitVec 32)) (store |%1-@rhs| ((_ extract 0 0) (_ bv1 32)) |%b-@rhs|))
;; %3 = extractelement <2 x i32> %2, i32 0
(define-fun |%3-@rhs| () (_ BitVec 32) (select |%2-@rhs| ((_ extract 0 0) (_ bv0 32))))
;; ret i32 %3
;; No backward arrows


(define-fun |-@rhs_result| () (_ BitVec 32) |%3-@rhs|)

;; Function: |-@main|
;; (i32 %argc, i8** %argv)
(declare-fun memory3 () Mem)
(define-fun rsp3 () (_ BitVec 64) (_ bv0 64))
(declare-fun |%argc-@main| () (_ BitVec 32))
(declare-fun |%argv-@main| () (_ BitVec 64))

;; BLOCK %0 with index 0 and rank = 1
;; Predecessors:
;; |-@main_block_0_entry_condition| 
(define-fun |-@main_block_0_entry_condition| () Bool true)
;; %1 = alloca i32, align 4
(define-fun rsp4 () Address (bvsub rsp3 (_ bv4 64)))
(define-fun |%1-@main| () (_ BitVec 64) rsp4)
;; %2 = alloca i32, align 4
(define-fun rsp5 () Address (bvsub rsp4 (_ bv4 64)))
(define-fun |%2-@main| () (_ BitVec 64) rsp5)
;; %3 = alloca i8**, align 8
(define-fun rsp6 () Address (bvsub rsp5 (_ bv8 64)))
(define-fun |%3-@main| () (_ BitVec 64) rsp6)
;; %a = alloca i32, align 4
(define-fun rsp7 () Address (bvsub rsp6 (_ bv4 64)))
(define-fun |%a-@main| () (_ BitVec 64) rsp7)
;; %b = alloca i32, align 4
(define-fun rsp8 () Address (bvsub rsp7 (_ bv4 64)))
(define-fun |%b-@main| () (_ BitVec 64) rsp8)
;; %lhs = alloca i32, align 4
(define-fun rsp9 () Address (bvsub rsp8 (_ bv4 64)))
(define-fun |%lhs-@main| () (_ BitVec 64) rsp9)
;; %rhs = alloca i32, align 4
(define-fun rsp10 () Address (bvsub rsp9 (_ bv4 64)))
(define-fun |%rhs-@main| () (_ BitVec 64) rsp10)
;; store i32 0, i32* %1
(define-fun memory4 () Mem (write32 memory3 |%1-@main| (_ bv0 32)))
;; store i32 %argc, i32* %2, align 4
(define-fun memory5 () Mem (write32 memory4 |%2-@main| |%argc-@main|))
;; store i8** %argv, i8*** %3, align 8
(define-fun memory6 () Mem (write64 memory5 |%3-@main| |%argv-@main|))
;; %4 = load i32* %2, align 4
(define-fun |%4-@main| () (_ BitVec 32) (read32 memory6 |%2-@main|))
;; %5 = icmp eq i32 %4, 3
(define-fun |%5-@main| () Bool (= |%4-@main| (_ bv3 32)))
;; br i1 %5, label %6, label %32
;; No backward arrows

;; BLOCK %6 with index 1 and rank = 2
;; Predecessors: %0
;; |-@main_block_1_entry_condition| 
(define-fun |-@main_block_1_entry_condition| () Bool
    (and |-@main_block_0_entry_condition| |%5-@main|)
)
;;Memory PHI
(define-fun memory7 () Mem memory6)
;; %7 = load i8*** %3, align 8
(define-fun |%7-@main| () (_ BitVec 64) (read64 memory7 |%3-@main|))
;; %8 = getelementptr inbounds i8** %7, i64 1
(define-fun |%8-@main| () (_ BitVec 64) (bvadd |%7-@main| (_ bv8 64)))
;; %9 = load i8** %8, align 8
(define-fun |%9-@main| () (_ BitVec 64) (read64 memory7 |%8-@main|))
;; %10 = call i32 @atoi(i8* %9)
(declare-fun |%10-@main| () (_ BitVec 32))
;; store i32 %10, i32* %a, align 4
(define-fun memory8 () Mem (write32 memory7 |%a-@main| |%10-@main|))
;; %11 = load i8*** %3, align 8
(define-fun |%11-@main| () (_ BitVec 64) (read64 memory8 |%3-@main|))
;; %12 = getelementptr inbounds i8** %11, i64 2
(define-fun |%12-@main| () (_ BitVec 64) (bvadd |%11-@main| (_ bv16 64)))
;; %13 = load i8** %12, align 8
(define-fun |%13-@main| () (_ BitVec 64) (read64 memory8 |%12-@main|))
;; %14 = call i32 @atoi(i8* %13)
(declare-fun |%14-@main| () (_ BitVec 32))
;; store i32 %14, i32* %b, align 4
(define-fun memory9 () Mem (write32 memory8 |%b-@main| |%14-@main|))
;; %15 = load i32* %a, align 4
(define-fun |%15-@main| () (_ BitVec 32) (read32 memory9 |%a-@main|))
;; %16 = load i32* %b, align 4
(define-fun |%16-@main| () (_ BitVec 32) (read32 memory9 |%b-@main|))
;; %17 = call i32 @lhs(i32 %15, i32 %16)
(declare-fun |%17-@main| () (_ BitVec 32))
;; store i32 %17, i32* %lhs, align 4
(define-fun memory10 () Mem (write32 memory9 |%lhs-@main| |%17-@main|))
;; %18 = load i32* %a, align 4
(define-fun |%18-@main| () (_ BitVec 32) (read32 memory10 |%a-@main|))
;; %19 = load i32* %b, align 4
(define-fun |%19-@main| () (_ BitVec 32) (read32 memory10 |%b-@main|))
;; %20 = call i32 @rhs(i32 %18, i32 %19)
(declare-fun |%20-@main| () (_ BitVec 32))
;; store i32 %20, i32* %rhs, align 4
(define-fun memory11 () Mem (write32 memory10 |%rhs-@main| |%20-@main|))
;; %21 = load i32* %lhs, align 4
(define-fun |%21-@main| () (_ BitVec 32) (read32 memory11 |%lhs-@main|))
;; %22 = load i32* %rhs, align 4
(define-fun |%22-@main| () (_ BitVec 32) (read32 memory11 |%rhs-@main|))
;; %23 = icmp eq i32 %21, %22
(define-fun |%23-@main| () Bool (= |%21-@main| |%22-@main|))
;; br i1 %23, label %24, label %27
;; No backward arrows

;; BLOCK %24 with index 2 and rank = 3
;; Predecessors: %6
;; |-@main_block_2_entry_condition| 
(define-fun |-@main_block_2_entry_condition| () Bool
    (and |-@main_block_1_entry_condition| |%23-@main|)
)
;;Memory PHI
(define-fun memory12 () Mem memory11)
;; %25 = load i32* %lhs, align 4
(define-fun |%25-@main| () (_ BitVec 32) (read32 memory12 |%lhs-@main|))
;; %26 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @.str, i32 0, i32 0), i32 %25)
(declare-fun |%26-@main| () (_ BitVec 32))
;; br label %31
;; No backward arrows

;; BLOCK %27 with index 3 and rank = 3
;; Predecessors: %6
;; |-@main_block_3_entry_condition| 
(define-fun |-@main_block_3_entry_condition| () Bool
    (and |-@main_block_1_entry_condition| (not |%23-@main|))
)
;;Memory PHI
(define-fun memory13 () Mem memory11)
;; %28 = load i32* %lhs, align 4
(define-fun |%28-@main| () (_ BitVec 32) (read32 memory13 |%lhs-@main|))
;; %29 = load i32* %rhs, align 4
(define-fun |%29-@main| () (_ BitVec 32) (read32 memory13 |%rhs-@main|))
;; %30 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @.str1, i32 0, i32 0), i32 %28, i32 %29)
(declare-fun |%30-@main| () (_ BitVec 32))
;; br label %31
;; No backward arrows

;; BLOCK %31 with index 4 and rank = 4
;; Predecessors: %27 %24
;; |-@main_block_4_entry_condition| 
(define-fun |-@main_block_4_entry_condition| () Bool
    (or
        |-@main_block_3_entry_condition|
        |-@main_block_2_entry_condition|
    )
)
;;Memory PHI
(define-fun memory14 () Mem 
    (ite |-@main_block_3_entry_condition| memory13 memory12
    ))
;; br label %32
;; No backward arrows

;; BLOCK %32 with index 5 and rank = 5
;; Predecessors: %31 %0
;; |-@main_block_5_entry_condition| 
(define-fun |-@main_block_5_entry_condition| () Bool
    (or
        |-@main_block_4_entry_condition|
        (and |-@main_block_0_entry_condition| (not |%5-@main|))
    )
)
;;Memory PHI
(define-fun memory15 () Mem 
    (ite |-@main_block_4_entry_condition| memory14 memory6
    ))
;; ret i32 0
;; No backward arrows


(define-fun |-@main_result| () (_ BitVec 32) (_ bv0 32))

;; Function: |-@atoi|
;; (i8*)


;; Function: |-@printf|
;; (i8*, ...)


(assert (and (= |%a-@lhs| |%a-@rhs|) (= |%b-@lhs| |%b-@rhs|) (not (= |-@lhs_result| |-@rhs_result|))))

(check-sat)

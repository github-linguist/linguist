;; The .pox contract
;; Error codes
(define-constant ERR_STACKING_UNREACHABLE 255)
(define-constant ERR_STACKING_INSUFFICIENT_FUNDS 1)
(define-constant ERR_STACKING_INVALID_LOCK_PERIOD 2)
(define-constant ERR_STACKING_ALREADY_STACKED 3)
(define-constant ERR_STACKING_NO_SUCH_PRINCIPAL 4)
(define-constant ERR_STACKING_EXPIRED 5)
(define-constant ERR_STACKING_STX_LOCKED 6)
(define-constant ERR_STACKING_PERMISSION_DENIED 9)
(define-constant ERR_STACKING_THRESHOLD_NOT_MET 11)
(define-constant ERR_STACKING_POX_ADDRESS_IN_USE 12)
(define-constant ERR_STACKING_INVALID_POX_ADDRESS 13)
(define-constant ERR_STACKING_ALREADY_REJECTED 17)
(define-constant ERR_STACKING_INVALID_AMOUNT 18)
(define-constant ERR_NOT_ALLOWED 19)
(define-constant ERR_STACKING_ALREADY_DELEGATED 20)
(define-constant ERR_DELEGATION_EXPIRES_DURING_LOCK 21)
(define-constant ERR_DELEGATION_TOO_MUCH_LOCKED 22)
(define-constant ERR_DELEGATION_POX_ADDR_REQUIRED 23)
(define-constant ERR_INVALID_START_BURN_HEIGHT 24)

;; PoX disabling threshold (a percent)
(define-constant POX_REJECTION_FRACTION u25)

;; Data vars that store a copy of the burnchain configuration.
;; Implemented as data-vars, so that different configurations can be
;; used in e.g. test harnesses.
(define-data-var pox-prepare-cycle-length uint PREPARE_CYCLE_LENGTH)
(define-data-var pox-reward-cycle-length uint REWARD_CYCLE_LENGTH)
(define-data-var pox-rejection-fraction uint POX_REJECTION_FRACTION)
(define-data-var first-burnchain-block-height uint u0)
(define-data-var configured bool false)

;; This function can only be called once, when it boots up
(define-public (set-burnchain-parameters (first-burn-height uint) (prepare-cycle-length uint) (reward-cycle-length uint) (rejection-fraction uint))
    (begin
        (asserts! (not (var-get configured)) (err ERR_NOT_ALLOWED))
        (var-set first-burnchain-block-height first-burn-height)
        (var-set pox-prepare-cycle-length prepare-cycle-length)
        (var-set pox-reward-cycle-length reward-cycle-length)
        (var-set pox-rejection-fraction rejection-fraction)
        (var-set configured true)
        (ok true))
)

;; The Stacking lock-up state and associated metadata.
;; Records can be inserted into this map via one of two ways:
;; * via contract-call? to the (stack-stx) method, or
;; * via a transaction in the underlying burnchain that encodes the same data.
;; In the latter case, this map will be updated by the Stacks
;; node itself, and transactions in the burnchain will take priority
;; over transactions in the Stacks chain when processing this block.
(define-map stacking-state
    { stacker: principal }
    {
        ;; how many uSTX locked?
        amount-ustx: uint,
        ;; Description of the underlying burnchain address that will
        ;; receive PoX'ed tokens. Translating this into an address
        ;; depends on the burnchain being used.  When Bitcoin is
        ;; the burnchain, this gets translated into a p2pkh, p2sh,
        ;; p2wpkh-p2sh, or p2wsh-p2sh UTXO, depending on the version.
        pox-addr: { version: (buff 1), hashbytes: (buff 20) },
        ;; how long the uSTX are locked, in reward cycles.
        lock-period: uint,
        ;; reward cycle when rewards begin
        first-reward-cycle: uint
    }
)

;; Delegation relationships
(define-map delegation-state
    { stacker: principal }
    {
        amount-ustx: uint,              ;; how many uSTX delegated?
        delegated-to: principal,        ;; who are we delegating?
        until-burn-ht: (optional uint), ;; how long does the delegation last?
        ;; does the delegate _need_ to use a specific
        ;; pox recipient address?
        pox-addr: (optional { version: (buff 1), hashbytes: (buff 20) })
    }
)

;; allowed contract-callers
(define-map allowance-contract-callers
    { sender: principal, contract-caller: principal }
    { until-burn-ht: (optional uint) })

;; How many uSTX are stacked in a given reward cycle.
;; Updated when a new PoX address is registered, or when more STX are granted
;; to it.
(define-map reward-cycle-total-stacked
    { reward-cycle: uint }
    { total-ustx: uint }
)

;; Internal map read by the Stacks node to iterate through the list of
;; PoX reward addresses on a per-reward-cycle basis.
(define-map reward-cycle-pox-address-list
    { reward-cycle: uint, index: uint }
    {
        pox-addr: { version: (buff 1), hashbytes: (buff 20) },
        total-ustx: uint
    }
)

(define-map reward-cycle-pox-address-list-len
    { reward-cycle: uint }
    { len: uint }
)

;; how much has been locked up for this address before
;;   committing?
;; this map allows stackers to stack amounts < minimum
;;   by paying the cost of aggregation during the commit
(define-map partial-stacked-by-cycle
    {
        pox-addr: { version: (buff 1), hashbytes: (buff 20) },
        reward-cycle: uint,
        sender: principal
    }
    { stacked-amount: uint }
)

;; Amount of uSTX that reject PoX, by reward cycle
(define-map stacking-rejection
    { reward-cycle: uint }
    { amount: uint }
)

;; Who rejected in which reward cycle
(define-map stacking-rejectors
    { stacker: principal, reward-cycle: uint }
    { amount: uint }
)

;; Getter for stacking-rejectors
(define-read-only (get-pox-rejection (stacker principal) (reward-cycle uint))
    (map-get? stacking-rejectors { stacker: stacker, reward-cycle: reward-cycle }))

;; Has PoX been rejected in the given reward cycle?
(define-read-only (is-pox-active (reward-cycle uint))
    (let (
        (reject-votes
            (default-to
                u0
                (get amount (map-get? stacking-rejection { reward-cycle: reward-cycle }))))
    )
    ;; (100 * reject-votes) / stx-liquid-supply < pox-rejection-fraction
    (< (* u100 reject-votes)
       (* (var-get pox-rejection-fraction) stx-liquid-supply)))
)

;; What's the reward cycle number of the burnchain block height?
;; Will runtime-abort if height is less than the first burnchain block (this is intentional)
(define-private (burn-height-to-reward-cycle (height uint))
    (/ (- height (var-get first-burnchain-block-height)) (var-get pox-reward-cycle-length)))

;; What's the block height at the start of a given reward cycle?
(define-private (reward-cycle-to-burn-height (cycle uint))
    (+ (var-get first-burnchain-block-height) (* cycle (var-get pox-reward-cycle-length))))

;; What's the current PoX reward cycle?
(define-private (current-pox-reward-cycle)
    (burn-height-to-reward-cycle burn-block-height))

;; Get the _current_ PoX stacking principal information.  If the information
;; is expired, or if there's never been such a stacker, then returns none.
(define-read-only (get-stacker-info (stacker principal))
    (match (map-get? stacking-state { stacker: stacker })
        stacking-info
            (if (<= (+ (get first-reward-cycle stacking-info) (get lock-period stacking-info)) (current-pox-reward-cycle))
                ;; present, but lock has expired
                none
                ;; present, and lock has not expired
                (some stacking-info)
            )
        ;; no state at all
        none
    ))

(define-private (check-caller-allowed)
    (or (is-eq tx-sender contract-caller)
        (let ((caller-allowed
                 ;; if not in the caller map, return false
                 (unwrap! (map-get? allowance-contract-callers
                                    { sender: tx-sender, contract-caller: contract-caller })
                          false)))
          ;; is the caller allowance expired?
          (if (< burn-block-height (unwrap! (get until-burn-ht caller-allowed) true))
              false
              true))))

(define-private (get-check-delegation (stacker principal))
    (let ((delegation-info (try! (map-get? delegation-state { stacker: stacker }))))
      ;; did the existing delegation expire?
      (if (match (get until-burn-ht delegation-info)
                 until-burn-ht (> burn-block-height until-burn-ht)
                 false)
          ;; it expired, return none
          none
          ;; delegation is active
          (some delegation-info))))

;; Get the size of the reward set for a reward cycle.
;; Note that this does _not_ return duplicate PoX addresses.
;; Note that this also _will_ return PoX addresses that are beneath
;; the minimum threshold -- i.e. the threshold can increase after insertion.
;; Used internally by the Stacks node, which filters out the entries
;; in this map to select PoX addresses with enough STX.
(define-read-only (get-reward-set-size (reward-cycle uint))
    (default-to
        u0
        (get len (map-get? reward-cycle-pox-address-list-len { reward-cycle: reward-cycle }))))

;; How many rejection votes have we been accumulating for the next block
(define-private (next-cycle-rejection-votes)
    (default-to
        u0
        (get amount (map-get? stacking-rejection { reward-cycle: (+ u1 (current-pox-reward-cycle)) }))))

;; Add a single PoX address to a single reward cycle.
;; Used to build up a set of per-reward-cycle PoX addresses.
;; No checking will be done -- don't call if this PoX address is already registered in this reward cycle!
(define-private (append-reward-cycle-pox-addr (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                                              (reward-cycle uint)
                                              (amount-ustx uint))
    (let (
        (sz (get-reward-set-size reward-cycle))
    )
    (map-set reward-cycle-pox-address-list
        { reward-cycle: reward-cycle, index: sz }
        { pox-addr: pox-addr, total-ustx: amount-ustx })
    (map-set reward-cycle-pox-address-list-len
        { reward-cycle: reward-cycle }
        { len: (+ u1 sz) })
    (+ u1 sz))
)

;; How many uSTX are stacked?
(define-read-only (get-total-ustx-stacked (reward-cycle uint))
    (default-to
        u0
        (get total-ustx (map-get? reward-cycle-total-stacked { reward-cycle: reward-cycle })))
)

;; Called internally by the node to iterate through the list of PoX addresses in this reward cycle.
;; Returns (optional (tuple (pox-addr <pox-address>) (total-ustx <uint>)))
(define-read-only (get-reward-set-pox-address (reward-cycle uint) (index uint))
    (map-get? reward-cycle-pox-address-list { reward-cycle: reward-cycle, index: index }))

;; Add a PoX address to the ith reward cycle, if i is between 0 and the given num-cycles (exclusive).
;; Arguments are given as a tuple, so this function can be (map ..)'ed onto a list of its arguments.
;; Used by add-pox-addr-to-reward-cycles.
;; No checking is done.
;; Returns 1 if added.
;; Returns 0 if not added.
(define-private (add-pox-addr-to-ith-reward-cycle (cycle-index uint) (params (tuple
                                                            (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                                                            (first-reward-cycle uint)
                                                            (num-cycles uint)
                                                            (amount-ustx uint)
                                                            (i uint))))
    (let ((reward-cycle (+ (get first-reward-cycle params) (get i params)))
          (num-cycles (get num-cycles params))
          (i (get i params)))
    {
        pox-addr: (get pox-addr params),
        first-reward-cycle: (get first-reward-cycle params),
        num-cycles: num-cycles,
        amount-ustx: (get amount-ustx params),
        i: (if (< i num-cycles)
            (let ((total-ustx (get-total-ustx-stacked reward-cycle)))
              ;; record how many uSTX this pox-addr will stack for in the given reward cycle
              (append-reward-cycle-pox-addr
                (get pox-addr params)
                reward-cycle
                (get amount-ustx params))

              ;; update running total
              (map-set reward-cycle-total-stacked
                 { reward-cycle: reward-cycle }
                 { total-ustx: (+ (get amount-ustx params) total-ustx) })

              ;; updated _this_ reward cycle
              (+ i u1))
            (+ i u0))
    }))

;; Add a PoX address to a given sequence of reward cycle lists.
;; A PoX address can be added to at most 12 consecutive cycles.
;; No checking is done.
(define-private (add-pox-addr-to-reward-cycles (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                                               (first-reward-cycle uint)
                                               (num-cycles uint)
                                               (amount-ustx uint))
  (let ((cycle-indexes (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11)))
    ;; For safety, add up the number of times (add-principal-to-ith-reward-cycle) returns 1.
    ;; It _should_ be equal to num-cycles.
    (asserts!
     (is-eq num-cycles
            (get i (fold add-pox-addr-to-ith-reward-cycle cycle-indexes
                         { pox-addr: pox-addr, first-reward-cycle: first-reward-cycle, num-cycles: num-cycles, amount-ustx: amount-ustx, i: u0 })))
     (err ERR_STACKING_UNREACHABLE))
    (ok true)))

(define-private (add-pox-partial-stacked-to-ith-cycle
                 (cycle-index uint)
                 (params { pox-addr: { version: (buff 1), hashbytes: (buff 20) },
                           reward-cycle: uint,
                           num-cycles: uint,
                           amount-ustx: uint }))
  (let ((pox-addr     (get pox-addr     params))
        (num-cycles   (get num-cycles   params))
        (reward-cycle (get reward-cycle params))
        (amount-ustx  (get amount-ustx  params)))
    (let ((current-amount
           (default-to u0
             (get stacked-amount
                  (map-get? partial-stacked-by-cycle { sender: tx-sender, pox-addr: pox-addr, reward-cycle: reward-cycle })))))
      (if (>= cycle-index num-cycles)
          ;; do not add to cycles >= cycle-index
          false
          ;; otherwise, add to the partial-stacked-by-cycle
          (map-set partial-stacked-by-cycle
                   { sender: tx-sender, pox-addr: pox-addr, reward-cycle: reward-cycle }
                   { stacked-amount: (+ amount-ustx current-amount) }))
      ;; produce the next params tuple
      { pox-addr: pox-addr,
        reward-cycle: (+ u1 reward-cycle),
        num-cycles: num-cycles,
        amount-ustx: amount-ustx })))

;; Add a PoX address to a given sequence of partial reward cycle lists.
;; A PoX address can be added to at most 12 consecutive cycles.
;; No checking is done.
(define-private (add-pox-partial-stacked (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                                         (first-reward-cycle uint)
                                         (num-cycles uint)
                                         (amount-ustx uint))
  (let ((cycle-indexes (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11)))
    (fold add-pox-partial-stacked-to-ith-cycle cycle-indexes
          { pox-addr: pox-addr, reward-cycle: first-reward-cycle, num-cycles: num-cycles, amount-ustx: amount-ustx })
    true))

;; What is the minimum number of uSTX to be stacked in the given reward cycle?
;; Used internally by the Stacks node, and visible publicly.
(define-read-only (get-stacking-minimum)
    (/ stx-liquid-supply STACKING_THRESHOLD_25))

;; Is the address mode valid for a PoX burn address?
(define-private (check-pox-addr-version (version (buff 1)))
    (or (is-eq version ADDRESS_VERSION_P2PKH)
        (is-eq version ADDRESS_VERSION_P2SH)
        (is-eq version ADDRESS_VERSION_P2WPKH)
        (is-eq version ADDRESS_VERSION_P2WSH)))

;; Is the given lock period valid?
(define-private (check-pox-lock-period (lock-period uint))
    (and (>= lock-period MIN_POX_REWARD_CYCLES)
         (<= lock-period MAX_POX_REWARD_CYCLES)))

;; Evaluate if a participant can stack an amount of STX for a given period.
;; This method is designed as a read-only method so that it can be used as
;; a set of guard conditions and also as a read-only RPC call that can be
;; performed beforehand.
(define-read-only (can-stack-stx (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                                  (amount-ustx uint)
                                  (first-reward-cycle uint)
                                  (num-cycles uint))
  (begin
    ;; minimum uSTX must be met
    (asserts! (<= (print (get-stacking-minimum)) amount-ustx)
              (err ERR_STACKING_THRESHOLD_NOT_MET))

    (minimal-can-stack-stx pox-addr amount-ustx first-reward-cycle num-cycles)))

;; Evaluate if a participant can stack an amount of STX for a given period.
;; This method is designed as a read-only method so that it can be used as
;; a set of guard conditions and also as a read-only RPC call that can be
;; performed beforehand.
(define-read-only (minimal-can-stack-stx
                   (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                   (amount-ustx uint)
                   (first-reward-cycle uint)
                   (num-cycles uint))
  (begin
    ;; amount must be valid
    (asserts! (> amount-ustx u0)
              (err ERR_STACKING_INVALID_AMOUNT))

    ;; sender principal must not have rejected in this upcoming reward cycle
    (asserts! (is-none (get-pox-rejection tx-sender first-reward-cycle))
              (err ERR_STACKING_ALREADY_REJECTED))

    ;; lock period must be in acceptable range.
    (asserts! (check-pox-lock-period num-cycles)
              (err ERR_STACKING_INVALID_LOCK_PERIOD))

    ;; address version must be valid
    (asserts! (check-pox-addr-version (get version pox-addr))
              (err ERR_STACKING_INVALID_POX_ADDRESS))
    (ok true)))

;; Revoke contract-caller authorization to call stacking methods
(define-public (disallow-contract-caller (caller principal))
  (begin
    (asserts! (is-eq tx-sender contract-caller)
              (err ERR_STACKING_PERMISSION_DENIED))
    (ok (map-delete allowance-contract-callers { sender: tx-sender, contract-caller: caller }))))

;; Give a contract-caller authorization to call stacking methods
;;  normally, stacking methods may only be invoked by _direct_ transactions
;;   (i.e., the tx-sender issues a direct contract-call to the stacking methods)
;;  by issuing an allowance, the tx-sender may call through the allowed contract
(define-public (allow-contract-caller (caller principal) (until-burn-ht (optional uint)))
  (begin
    (asserts! (is-eq tx-sender contract-caller)
              (err ERR_STACKING_PERMISSION_DENIED))
    (ok (map-set allowance-contract-callers
               { sender: tx-sender, contract-caller: caller }
               { until-burn-ht: until-burn-ht }))))

;; Lock up some uSTX for stacking!  Note that the given amount here is in micro-STX (uSTX).
;; The STX will be locked for the given number of reward cycles (lock-period).
;; This is the self-service interface.  tx-sender will be the Stacker.
;;
;; * The given stacker cannot currently be stacking.
;; * You will need the minimum uSTX threshold.  This will be determined by (get-stacking-minimum)
;; at the time this method is called.
;; * You may need to increase the amount of uSTX locked up later, since the minimum uSTX threshold
;; may increase between reward cycles.
;; * The Stacker will receive rewards in the reward cycle following `start-burn-ht`.
;; Importantly, `start-burn-ht` may not be further into the future than the next reward cycle,
;; and in most cases should be set to the current burn block height.
;;
;; The tokens will unlock and be returned to the Stacker (tx-sender) automatically.
(define-public (stack-stx (amount-ustx uint)
                          (pox-addr (tuple (version (buff 1)) (hashbytes (buff 20))))
                          (start-burn-ht uint)
                          (lock-period uint))
    ;; this stacker's first reward cycle is the _next_ reward cycle
    (let ((first-reward-cycle (+ u1 (current-pox-reward-cycle)))
          (specified-reward-cycle (+ u1 (burn-height-to-reward-cycle start-burn-ht))))
      ;; the start-burn-ht must result in the next reward cycle, do not allow stackers
      ;;  to "post-date" their `stack-stx` transaction
      (asserts! (is-eq first-reward-cycle specified-reward-cycle)
                (err ERR_INVALID_START_BURN_HEIGHT))

      ;; must be called directly by the tx-sender or by an allowed contract-caller
      (asserts! (check-caller-allowed)
                (err ERR_STACKING_PERMISSION_DENIED))

      ;; tx-sender principal must not be stacking
      (asserts! (is-none (get-stacker-info tx-sender))
        (err ERR_STACKING_ALREADY_STACKED))

      ;; tx-sender must not be delegating
      (asserts! (is-none (get-check-delegation tx-sender))
        (err ERR_STACKING_ALREADY_DELEGATED))

      ;; the Stacker must have sufficient unlocked funds
      (asserts! (>= (stx-get-balance tx-sender) amount-ustx)
        (err ERR_STACKING_INSUFFICIENT_FUNDS))

      ;; ensure that stacking can be performed
      (try! (can-stack-stx pox-addr amount-ustx first-reward-cycle lock-period))

      ;; register the PoX address with the amount stacked
      (try! (add-pox-addr-to-reward-cycles pox-addr first-reward-cycle lock-period amount-ustx))

      ;; add stacker record
      (map-set stacking-state
        { stacker: tx-sender }
        { amount-ustx: amount-ustx,
          pox-addr: pox-addr,
          first-reward-cycle: first-reward-cycle,
          lock-period: lock-period })

      ;; return the lock-up information, so the node can actually carry out the lock.
      (ok { stacker: tx-sender, lock-amount: amount-ustx, unlock-burn-height: (reward-cycle-to-burn-height (+ first-reward-cycle lock-period)) }))
)

(define-public (revoke-delegate-stx)
  (begin
    ;; must be called directly by the tx-sender or by an allowed contract-caller
    (asserts! (check-caller-allowed)
              (err ERR_STACKING_PERMISSION_DENIED))
    (ok (map-delete delegation-state { stacker: tx-sender }))))

;; Delegate to `delegate-to` the ability to stack from a given address.
;;  This method _does not_ lock the funds, rather, it allows the delegate
;;  to issue the stacking lock.
;; The caller specifies:
;;   * amount-ustx: the total amount of ustx the delegate may be allowed to lock
;;   * until-burn-ht: an optional burn height at which this delegation expiration
;;   * pox-addr: an optional address to which any rewards *must* be sent
(define-public (delegate-stx (amount-ustx uint)
                             (delegate-to principal)
                             (until-burn-ht (optional uint))
                             (pox-addr (optional { version: (buff 1),
                                                   hashbytes: (buff 20) })))
    (begin
      ;; must be called directly by the tx-sender or by an allowed contract-caller
      (asserts! (check-caller-allowed)
                (err ERR_STACKING_PERMISSION_DENIED))

      ;; tx-sender principal must not be stacking
      (asserts! (is-none (get-stacker-info tx-sender))
        (err ERR_STACKING_ALREADY_STACKED))

      ;; tx-sender must not be delegating
      (asserts! (is-none (get-check-delegation tx-sender))
        (err ERR_STACKING_ALREADY_DELEGATED))

      ;; add delegation record
      (map-set delegation-state
        { stacker: tx-sender }
        { amount-ustx: amount-ustx,
          delegated-to: delegate-to,
          until-burn-ht: until-burn-ht,
          pox-addr: pox-addr })

      (ok true)))

;; Commit partially stacked STX.
;;   This allows a stacker/delegate to lock fewer STX than the minimal threshold in multiple transactions,
;;   so long as: 1. The pox-addr is the same.
;;               2. This "commit" transaction is called _before_ the PoX anchor block.
;;   This ensures that each entry in the reward set returned to the stacks-node is greater than the threshold,
;;   but does not require it be all locked up within a single transaction
(define-public (stack-aggregation-commit (pox-addr { version: (buff 1), hashbytes: (buff 20) })
                                         (reward-cycle uint))
  (let ((partial-stacked
         ;; fetch the partial commitments
         (unwrap! (map-get? partial-stacked-by-cycle { pox-addr: pox-addr, sender: tx-sender, reward-cycle: reward-cycle })
                  (err ERR_STACKING_NO_SUCH_PRINCIPAL))))
    ;; must be called directly by the tx-sender or by an allowed contract-caller
    (asserts! (check-caller-allowed)
              (err ERR_STACKING_PERMISSION_DENIED))
    (let ((amount-ustx (get stacked-amount partial-stacked)))
      (try! (can-stack-stx pox-addr amount-ustx reward-cycle u1))
      ;; add the pox addr to the reward cycle
      (add-pox-addr-to-ith-reward-cycle
       u0
       { pox-addr: pox-addr,
         first-reward-cycle: reward-cycle,
         num-cycles: u1,
         amount-ustx: amount-ustx,
         i: u0 })
      ;; don't update the stacking-state map,
      ;;  because it _already has_ this stacker's state
      ;; don't lock the STX, because the STX is already locked
      ;;
      ;; clear the partial-stacked state
      (map-delete partial-stacked-by-cycle { pox-addr: pox-addr, sender: tx-sender, reward-cycle: reward-cycle })
      (ok true))))

;; As a delegate, stack the given principal's STX using partial-stacked-by-cycle
;; Once the delegate has stacked > minimum, the delegate should call stack-aggregation-commit
(define-public (delegate-stack-stx (stacker principal)
                                   (amount-ustx uint)
                                   (pox-addr { version: (buff 1), hashbytes: (buff 20) })
                                   (start-burn-ht uint)
                                   (lock-period uint))
    ;; this stacker's first reward cycle is the _next_ reward cycle
    (let ((first-reward-cycle (+ u1 (current-pox-reward-cycle)))
          (specified-reward-cycle (+ u1 (burn-height-to-reward-cycle start-burn-ht)))
          (unlock-burn-height (reward-cycle-to-burn-height (+ (current-pox-reward-cycle) u1 lock-period))))
      ;; the start-burn-ht must result in the next reward cycle, do not allow stackers
      ;;  to "post-date" their `stack-stx` transaction
      (asserts! (is-eq first-reward-cycle specified-reward-cycle)
                (err ERR_INVALID_START_BURN_HEIGHT))

      ;; must be called directly by the tx-sender or by an allowed contract-caller
      (asserts! (check-caller-allowed)
        (err ERR_STACKING_PERMISSION_DENIED))

      ;; stacker must have delegated to the caller
      (let ((delegation-info (unwrap! (get-check-delegation stacker) (err ERR_STACKING_PERMISSION_DENIED))))
        ;; must have delegated to tx-sender
        (asserts! (is-eq (get delegated-to delegation-info) tx-sender)
                  (err ERR_STACKING_PERMISSION_DENIED))
        ;; must have delegated enough stx
        (asserts! (>= (get amount-ustx delegation-info) amount-ustx)
                  (err ERR_DELEGATION_TOO_MUCH_LOCKED))
        ;; if pox-addr is set, must be equal to pox-addr
        (asserts! (match (get pox-addr delegation-info)
                         specified-pox-addr (is-eq pox-addr specified-pox-addr)
                         true)
                  (err ERR_DELEGATION_POX_ADDR_REQUIRED))
        ;; delegation must not expire before lock period
        (asserts! (match (get until-burn-ht delegation-info)
                         until-burn-ht (>= until-burn-ht
                                           unlock-burn-height)
                      true)
                  (err ERR_DELEGATION_EXPIRES_DURING_LOCK)))

      ;; stacker principal must not be stacking
      (asserts! (is-none (get-stacker-info stacker))
        (err ERR_STACKING_ALREADY_STACKED))

      ;; the Stacker must have sufficient unlocked funds
      (asserts! (>= (stx-get-balance stacker) amount-ustx)
        (err ERR_STACKING_INSUFFICIENT_FUNDS))

      ;; ensure that stacking can be performed
      (try! (minimal-can-stack-stx pox-addr amount-ustx first-reward-cycle lock-period))

      ;; register the PoX address with the amount stacked via partial stacking
      ;;   before it can be included in the reward set, this must be committed!
      (add-pox-partial-stacked pox-addr first-reward-cycle lock-period amount-ustx)

      ;; add stacker record
      (map-set stacking-state
        { stacker: stacker }
        { amount-ustx: amount-ustx,
          pox-addr: pox-addr,
          first-reward-cycle: first-reward-cycle,
          lock-period: lock-period })

      ;; return the lock-up information, so the node can actually carry out the lock.
      (ok { stacker: stacker,
            lock-amount: amount-ustx,
            unlock-burn-height: unlock-burn-height })))

;; Reject Stacking for this reward cycle.
;; tx-sender votes all its uSTX for rejection.
;; Note that unlike PoX, rejecting PoX does not lock the tx-sender's
;; tokens.  PoX rejection acts like a coin vote.
(define-public (reject-pox)
    (let (
        (balance (stx-get-balance tx-sender))
        (vote-reward-cycle (+ u1 (current-pox-reward-cycle)))
    )

    ;; tx-sender principal must not have rejected in this upcoming reward cycle
    (asserts! (is-none (get-pox-rejection tx-sender vote-reward-cycle))
        (err ERR_STACKING_ALREADY_REJECTED))

    ;; tx-sender can't be a stacker
    (asserts! (is-none (get-stacker-info tx-sender))
        (err ERR_STACKING_ALREADY_STACKED))

    ;; vote for rejection
    (map-set stacking-rejection
        { reward-cycle: vote-reward-cycle }
        { amount: (+ (next-cycle-rejection-votes) balance) }
    )

    ;; mark voted
    (map-set stacking-rejectors
        { stacker: tx-sender, reward-cycle: vote-reward-cycle }
        { amount: balance }
    )

    (ok true))
)

;; Used for PoX parameters discovery
(define-read-only (get-pox-info)
    (ok {
        min-amount-ustx: (get-stacking-minimum),
        reward-cycle-id: (current-pox-reward-cycle),
        prepare-cycle-length: (var-get pox-prepare-cycle-length),
        first-burnchain-block-height: (var-get first-burnchain-block-height),
        reward-cycle-length: (var-get pox-reward-cycle-length),
        rejection-fraction: (var-get pox-rejection-fraction),
        current-rejection-votes: (next-cycle-rejection-votes),
        total-liquid-supply-ustx: stx-liquid-supply,
    })
)

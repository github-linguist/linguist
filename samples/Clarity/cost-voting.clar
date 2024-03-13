;; The .cost-voting contract

;; error codes
(define-constant ERR_NO_SUCH_PROPOSAL        1)
(define-constant ERR_AMOUNT_NOT_POSITIVE     2)
(define-constant ERR_PROPOSAL_EXPIRED        3)
(define-constant ERR_VOTE_ENDED              4)
(define-constant ERR_INSUFFICIENT_FUNDS      5)
(define-constant ERR_FT_TRANSFER             6)
(define-constant ERR_STX_TRANSFER            7)
(define-constant ERR_VOTE_NOT_CONFIRMED      8)
(define-constant ERR_ALREADY_VETOED          9)
(define-constant ERR_NOT_LAST_MINER          10)
(define-constant ERR_INSUFFICIENT_VOTES      11)
(define-constant ERR_VETO_PERIOD_OVER        12)
(define-constant ERR_VETO_PERIOD_NOT_OVER    13)
(define-constant ERR_PROPOSAL_VETOED         14)
(define-constant ERR_PROPOSAL_CONFIRMED      15)
(define-constant ERR_FETCHING_BLOCK_INFO     16)
(define-constant ERR_TOO_MANY_CONFIRMED      17)
(define-constant ERR_UNREACHABLE             255)

(define-constant VOTE_LENGTH u2016)
(define-constant VETO_LENGTH u1008)
(define-constant REQUIRED_PERCENT_STX_VOTE u20)
(define-constant REQUIRED_VETOES u500)

(define-constant MAX_CONFIRMED_PER_BLOCK u10)

;; cost vote token
(define-fungible-token cost-vote-token)

;; proposal counters
(define-data-var proposal-count uint u0)
(define-data-var confirmed-proposal-count uint u0)

;; cost-function proposals
(define-map proposals
    { proposal-id: uint }
    {
        cost-function-contract: principal,
        cost-function-name: (string-ascii 128),
        function-contract: principal,
        function-name: (string-ascii 128),
        expiration-block-height: uint
    }
)

;; vote confirmed cost-function proposals
(define-map vote-confirmed-proposals
    { proposal-id: uint }
    { expiration-block-height: uint }
)

;; miner confirmed cost-function proposals
(define-map confirmed-proposals
   { confirmed-id: uint }
   {
       function-contract: principal,
       function-name: (string-ascii 128),
       cost-function-contract: principal,
       cost-function-name: (string-ascii 128),
       confirmed-height: uint
    }
)

;; limit the number of miner confirmed-proposals
;;   that can be introduced per block
;; track the # of proposals confirmed at a given block-height
(define-map confirmed-count-at-block uint uint)

(define-map proposal-confirmed-id
    { proposal-id: uint }
    { confirmed-id: uint }
)

(define-map functions-to-confirmed-ids
   { function-contract: principal, function-name: (string-ascii 128) }
   { proposal-id: uint }
)

;; cost-function proposal votes
(define-map proposal-votes { proposal-id: uint } { votes: uint })

;; cost-function proposal vetos
(define-map proposal-vetos { proposal-id: uint } { vetos: uint })

;; proposal vetos per block
(define-map exercised-veto { proposal-id: uint, veto-height: uint } { vetoed: bool })

;; the number of votes a specific principal has committed to a proposal
(define-map principal-proposal-votes { address: principal, proposal-id: uint } { votes: uint })

;; getter for cost-function proposals
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id }))

;; getter for confirmed cost-function proposals
(define-read-only (get-confirmed-proposal (confirmed-id uint))
    (map-get? confirmed-proposals { confirmed-id: confirmed-id }))

;; getter for cost-function proposal votes
(define-read-only (get-proposal-votes (proposal-id uint))
    (get votes (map-get? proposal-votes { proposal-id: proposal-id })))

;; getter for cost-function proposal vetos
(define-read-only (get-proposal-vetos (proposal-id uint))
    (get vetos (map-get? proposal-vetos { proposal-id: proposal-id })))

;; getter for cost-function proposal votes, for specific principal
(define-read-only (get-principal-votes (address principal) (proposal-id uint))
    (get votes (map-get? principal-proposal-votes { address: address, proposal-id: proposal-id })))

;; Propose cost-functions
(define-public (submit-proposal (function-contract principal)
                                (function-name (string-ascii 128))
                                (cost-function-contract principal)
                                (cost-function-name (string-ascii 128)))
    (begin
        (map-insert proposals { proposal-id: (var-get proposal-count) }
                              { cost-function-contract: cost-function-contract,
                                cost-function-name: cost-function-name,
                                function-contract: function-contract,
                                function-name: function-name,
                                expiration-block-height: (+ block-height VOTE_LENGTH) })
        (map-insert proposal-votes { proposal-id: (var-get proposal-count) } { votes: u0 })
        (var-set proposal-count (+ (var-get proposal-count) u1))
        (ok (- (var-get proposal-count) u1))))

;; Vote on a proposal
(define-public (vote-proposal (proposal-id uint) (amount uint))
    (let (
        (expiration-block-height (get expiration-block-height (unwrap! (map-get? proposals {
            proposal-id: proposal-id }) (err ERR_NO_SUCH_PROPOSAL))))
        (cur-votes (default-to u0 (get votes (map-get? proposal-votes { proposal-id: proposal-id }))))
        (cur-principal-votes (default-to u0 (get votes (map-get? principal-proposal-votes {
            address: tx-sender,
            proposal-id: proposal-id })))))

    ;; a vote must have a positive amount
    (asserts! (> amount u0) (err ERR_AMOUNT_NOT_POSITIVE))

    ;; the vote must occur before the expiration
    (asserts! (< block-height expiration-block-height) (err ERR_PROPOSAL_EXPIRED))

    ;; the proposal must not already be voter confirmed
    (asserts! (is-none (map-get? vote-confirmed-proposals { proposal-id: proposal-id }))
        (err ERR_VOTE_ENDED))

    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) (err ERR_INSUFFICIENT_FUNDS))
    (unwrap! (ft-mint? cost-vote-token amount tx-sender) (err ERR_UNREACHABLE))

    (map-set proposal-votes { proposal-id: proposal-id } { votes: (+ amount cur-votes) })
    (map-set principal-proposal-votes { address: tx-sender, proposal-id: proposal-id}
                                    { votes: (+ amount cur-principal-votes)})
    (ok true)))

;; Withdraw votes
(define-public (withdraw-votes (proposal-id uint) (amount uint))
    (let (
        (cur-votes (default-to u0 (get votes (map-get? proposal-votes { proposal-id: proposal-id }))))
        (cur-principal-votes (default-to u0 (get votes (map-get? principal-proposal-votes {
            address: tx-sender,
            proposal-id: proposal-id }))))
        (sender tx-sender))

    (asserts! (> amount u0) (err ERR_AMOUNT_NOT_POSITIVE))
    (asserts! (>= cur-principal-votes amount) (err ERR_INSUFFICIENT_FUNDS))

    (unwrap! (as-contract (stx-transfer? amount tx-sender sender)) (err ERR_STX_TRANSFER))
    (unwrap! (as-contract (ft-transfer? cost-vote-token amount sender tx-sender))
        (err ERR_FT_TRANSFER))

    (map-set proposal-votes { proposal-id: proposal-id } { votes: (- cur-votes amount) })
    (map-set principal-proposal-votes { address: tx-sender, proposal-id: proposal-id }
                                        { votes: (- cur-principal-votes amount) })
    (ok true)))

;; Miner veto
(define-public (veto (proposal-id uint))
    (let (
        (cur-vetos (default-to u0 (get vetos (map-get? proposal-vetos { proposal-id: proposal-id }))))
        (expiration-block-height (get expiration-block-height (unwrap!
            (map-get? vote-confirmed-proposals { proposal-id: proposal-id })
                (err ERR_VOTE_NOT_CONFIRMED))))
        (vetoed (default-to false (get vetoed (map-get? exercised-veto { proposal-id: proposal-id,
                                                                         veto-height: block-height }))))
        (last-miner (unwrap! (get-block-info? miner-address (- block-height u1))
            (err ERR_FETCHING_BLOCK_INFO))))

    ;; a miner can only veto once per block
    (asserts! (not vetoed) (err ERR_ALREADY_VETOED))

    ;; vetoes must be cast within the veto period
    (asserts! (< block-height expiration-block-height) (err ERR_VETO_PERIOD_OVER))

    ;; a miner can only veto if they mined the previous block
    (asserts! (is-eq contract-caller last-miner) (err ERR_NOT_LAST_MINER))

    ;; a veto cannot be cast if a proposal has already been miner confirmed
    (asserts! (is-none (map-get? proposal-confirmed-id { proposal-id: proposal-id }))
        (err ERR_PROPOSAL_CONFIRMED))

    (map-set proposal-vetos { proposal-id: proposal-id } { vetos: (+ u1 cur-vetos) })
    (map-set exercised-veto { proposal-id: proposal-id, veto-height: block-height }
                            { vetoed: true })
    (ok true)))

;; Confirm proposal has reached required vote count
(define-public (confirm-votes (proposal-id uint))
    (let (
        (votes (default-to u0 (get votes (map-get? proposal-votes { proposal-id: proposal-id }))))
        (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) (err ERR_NO_SUCH_PROPOSAL)))
        (confirmed-count (var-get confirmed-proposal-count))
        (expiration-block-height (get expiration-block-height proposal)))

    ;; confirmation fails if invoked after proposal has expired
    (asserts! (< block-height expiration-block-height) (err ERR_PROPOSAL_EXPIRED))

    ;; confirmation fails if the required threshold of votes is not met
    (asserts! (>= (/ (* votes u100) stx-liquid-supply) REQUIRED_PERCENT_STX_VOTE)
        (err ERR_INSUFFICIENT_VOTES))

    (map-insert vote-confirmed-proposals { proposal-id: proposal-id }
        { expiration-block-height: (+ VETO_LENGTH block-height) })

    (ok true)))

;; Confirm proposal hasn't been vetoed
(define-public (confirm-miners (proposal-id uint))
    (let ((vetos (default-to u0 (get vetos (map-get? proposal-vetos { proposal-id: proposal-id }))))
          (vote-confirmed-proposal (unwrap! (map-get? vote-confirmed-proposals
            { proposal-id: proposal-id }) (err ERR_NO_SUCH_PROPOSAL)))
          (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id })
            (err ERR_NO_SUCH_PROPOSAL)))
          (confirmed-count (var-get confirmed-proposal-count))
          (expiration-block-height (get expiration-block-height vote-confirmed-proposal))
          (confirmed-this-block (default-to u0 (map-get? confirmed-count-at-block block-height))))

    ;; have we already confirmed too many proposals in this block
    (asserts! (< confirmed-this-block MAX_CONFIRMED_PER_BLOCK) (err ERR_TOO_MANY_CONFIRMED))
    (map-set confirmed-count-at-block block-height (+ u1 confirmed-this-block))

    ;; miner confirmation will fail if invoked before the expiration
    (asserts! (>= block-height expiration-block-height) (err ERR_VETO_PERIOD_NOT_OVER))

    ;; miner confirmation will fail if there are enough vetos
    (asserts! (< vetos REQUIRED_VETOES) (err ERR_PROPOSAL_VETOED))

    (map-insert confirmed-proposals { confirmed-id: confirmed-count }
        {
            function-contract: (get function-contract proposal),
            function-name: (get function-name proposal),
            cost-function-contract: (get cost-function-contract proposal),
            cost-function-name: (get cost-function-name proposal),
            confirmed-height: block-height
        })

    (map-insert proposal-confirmed-id { proposal-id: proposal-id } { confirmed-id: confirmed-count })
    (var-set confirmed-proposal-count (+ confirmed-count u1))
    (ok true)))

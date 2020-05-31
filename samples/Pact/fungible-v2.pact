(interface fungible-v2

  " Standard for fungible coins and tokens as specified in KIP-0002. "

   ; ----------------------------------------------------------------------
   ; Schema

   (defschema account-details
    @doc "Schema for results of 'account' operation."
    @model [ (invariant (!= "" sender)) ]

    account:string
    balance:decimal
    guard:guard)


   ; ----------------------------------------------------------------------
   ; Caps

   (defcap TRANSFER:bool
     ( sender:string
       receiver:string
       amount:decimal
     )
     @doc " Managed capability sealing AMOUNT for transfer from SENDER to \
          \ RECEIVER. Permits any number of transfers up to AMOUNT."
     @managed amount TRANSFER-mgr
     )

   (defun TRANSFER-mgr:decimal
     ( managed:decimal
       requested:decimal
     )
     @doc " Manages TRANSFER AMOUNT linearly, \
          \ such that a request for 1.0 amount on a 3.0 \
          \ managed quantity emits updated amount 2.0."
     )

   ; ----------------------------------------------------------------------
   ; Functionality


  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal
    )
    @doc " Transfer AMOUNT between accounts SENDER and RECEIVER. \
         \ Fails if either SENDER or RECEIVER does not exist."
    @model [ (property (> amount 0.0))
             (property (!= sender ""))
             (property (!= receiver ""))
             (property (!= sender receiver))
           ]
    )

   (defun transfer-create:string
     ( sender:string
       receiver:string
       receiver-guard:guard
       amount:decimal
     )
     @doc " Transfer AMOUNT between accounts SENDER and RECEIVER. \
          \ Fails if SENDER does not exist. If RECEIVER exists, guard \
          \ must match existing value. If RECEIVER does not exist, \
          \ RECEIVER account is created using RECEIVER-GUARD. \
          \ Subject to management by TRANSFER capability."
     @model [ (property (> amount 0.0))
              (property (!= sender ""))
              (property (!= receiver ""))
              (property (!= sender receiver))
            ]
     )

   (defpact transfer-crosschain:string
     ( sender:string
       receiver:string
       receiver-guard:guard
       target-chain:string
       amount:decimal
     )
     @doc " 2-step pact to transfer AMOUNT from SENDER on current chain \
          \ to RECEIVER on TARGET-CHAIN via SPV proof. \
          \ TARGET-CHAIN must be different than current chain id. \
          \ First step debits AMOUNT coins in SENDER account and yields \
          \ RECEIVER, RECEIVER_GUARD and AMOUNT to TARGET-CHAIN. \
          \ Second step continuation is sent into TARGET-CHAIN with proof \
          \ obtained from the spv 'output' endpoint of Chainweb. \
          \ Proof is validated and RECEIVER is credited with AMOUNT \
          \ creating account with RECEIVER_GUARD as necessary."
     @model [ (property (> amount 0.0))
              (property (!= sender ""))
              (property (!= receiver ""))
              (property (!= sender receiver))
              (property (!= target-chain ""))
            ]
     )

   (defun get-balance:decimal
     ( account:string )
     " Get balance for ACCOUNT. Fails if account does not exist."
     )

   (defun details:object{account-details}
     ( account: string )
     " Get an object with details of ACCOUNT. \
     \ Fails if account does not exist."
     )

   (defun precision:integer
     ()
     "Return the maximum allowed decimal precision."
     )

   (defun enforce-unit:bool
     ( amount:decimal )
     " Enforce minimum precision allowed for transactions."
     )

   (defun create-account:string
     ( account:string
       guard:guard
     )
     " Create ACCOUNT with 0.0 balance, with GUARD controlling access."
     )

   (defun rotate:string
     ( account:string
       new-guard:guard
     )
     " Rotate guard for ACCOUNT. Transaction is validated against \
     \ existing guard before installing new guard. "
     )

)

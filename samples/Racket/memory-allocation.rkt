#lang racket
(collect-garbage) ; This function forces a garbage collection

(current-memory-use) ;Gives an estimate on the memory use based on the last garbage collection

(custodian-require-memory <limit-custodian>
                          <amount>
                          <stop-custodian>) ; Registers a check on required memory for the <limit-custodian>
                                            ; If amount of bytes can't be reached, <stop-custodian> is shutdown

(custodian-limit-memory <custodian> <amount>) ; Register a limit on memory for the <custodian>

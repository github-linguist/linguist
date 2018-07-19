hanoi = (ndisks, start_peg=1, end_peg=3) ->
  if ndisks
    staging_peg = 1 + 2 + 3 - start_peg - end_peg
    hanoi(ndisks-1, start_peg, staging_peg)
    console.log "Move disk #{ndisks} from peg #{start_peg} to #{end_peg}"
    hanoi(ndisks-1, staging_peg, end_peg)

hanoi(4)

repeat with beerCount from 99 to 1 by -1
  set bottles to "bottles"
  if beerCount < 99 then
    if beerCount = 1 then
      set bottles to "bottle"
    end
    log "" & beerCount & " " & bottles & " of beer on the wall"
    log ""
  end
  log "" & beerCount & " " & bottles & " of beer on the wall"
  log "" & beerCount & " " & bottles & " of beer"
  log "Take one down, pass it around"
end
log "No more bottles of beer on the wall!"

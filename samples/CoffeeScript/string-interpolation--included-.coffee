size = 'little'
console.log "Mary had a #{size} lamb." # Mary had a little lamb.
console.log "Escaping: \#{}" # Escaping: #{}
console.log 'No #{ interpolation} with single quotes' # No #{ interpolation} with single quotes

# Multi-line strings and arbtrary expressions work: 20
console.log """
  Multi-line strings and arbtrary expressions work: #{ 5 * 4 }
  """

cipher = (msg, rot) ->
  msg.replace /([a-z|A-Z])/g, ($1) ->
    c = $1.charCodeAt(0)
    String.fromCharCode \
      if c >= 97
      then (c + rot + 26 - 97) % 26 + 97
      else (c + rot + 26 - 65) % 26 + 65

console.log cipher "Hello World", 2
console.log cipher "azAz %^&*()", 3

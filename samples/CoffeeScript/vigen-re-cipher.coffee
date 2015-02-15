# Simple helper since charCodeAt is quite long to write.
code = (char) -> char.charCodeAt()

encrypt = (text, key) ->
	res = []
	j = 0
	
	for c in text.toUpperCase()
		continue if c < 'A' or c > 'Z'
		
		res.push ((code c) + (code key[j]) - 130) % 26 + 65
		j = ++j % key.length
	
	String.fromCharCode res...

decrypt = (text, key) ->
	res = []
	j = 0
	
	for c in text.toUpperCase()
		continue if c < 'A' or c > 'Z'
		
		res.push ((code c) - (code key[j]) + 26) % 26 + 65
		j = ++j % key.length
	
	String.fromCharCode res...

# Trying it out
key       = "VIGENERECIPHER"
original  = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
encrypted = encrypt original, key

console.log "Original  : #{original}"
console.log "Encrypted : #{encrypted}"
console.log "Decrypted : #{decrypt encrypted, key}"

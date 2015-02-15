def caeserEncode(int ​cipherKey, String text) {
    def builder = new StringBuilder()
    text.each { character ->
        int ch = character[0] as char
        switch(ch) {
            case 'a'..'z': ch = ((ch - 97 + cipherKey) % 26 + 97); break
            case 'A'..'Z': ch = ((ch - 65 + cipherKey) % 26 + 65); break
        }
        builder.append(ch as char)
    }
    builder.toString()
}
def caeserDecode(int cipherKey, String text) { caeserEncode(26 - cipherKey, text) }

def plainText = "The Quick Brown Fox jumped over the lazy dog"
def cipherKey = 12
def cipherText = caeserEncode(cipherKey, plainText)
def decodedText = caeserDecode(cipherKey, cipherText)

println "plainText: $plainText"
println "cypherText($cipherKey): $cipherText"
println "decodedText($cipherKey): $decodedText"

assert  plainText == decodedText​​

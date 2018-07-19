symbols = [ 1:'I', 4:'IV', 5:'V', 9:'IX', 10:'X', 40:'XL', 50:'L', 90:'XC', 100:'C', 400:'CD', 500:'D', 900:'CM', 1000:'M' ]

def roman(arabic) {
    def result = ""
    symbols.keySet().sort().reverse().each {
        while (arabic >= it) {
            arabic-=it
            result+=symbols[it]
        }
    }
    return result
}
assert roman(1) == 'I'
assert roman(2) == 'II'
assert roman(4) == 'IV'
assert roman(8) == 'VIII'
assert roman(16) == 'XVI'
assert roman(32) == 'XXXII'
assert roman(25) == 'XXV'
assert roman(64) == 'LXIV'
assert roman(128) == 'CXXVIII'
assert roman(256) == 'CCLVI'
assert roman(512) == 'DXII'
assert roman(954) == 'CMLIV'
assert roman(1024) == 'MXXIV'
assert roman(1666) == 'MDCLXVI'
assert roman(1990) == 'MCMXC'
assert roman(2008) == 'MMVIII'

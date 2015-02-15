def divMod(BigInteger number, BigInteger divisor) {
    def qr = number.divideAndRemainder(divisor)
    [div:qr[0], remainder:qr[1]]
}

def toText(value) {
    value = value as BigInteger
    def units = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',
            'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen']
    def tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety']
    def big = ['', 'thousand'] + ['m', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non', 'dec'].collect { "${it}illion"}

    if (value < 0) {
        "negative ${toText(-value)}"
    } else if (value < 20) {
        units[value]
    } else if (value < 100) {
        divMod(value, 10).with { "${tens[div]} ${units[remainder]}".replace(' zero', '') }
    } else if (value < 1000) {
        divMod(value, 100).with { "${toText(div)} hundred and ${toText(remainder)}".replace(' and zero', '') }
    } else {
        def chunks = []
        while (value != 0) {
            divMod(value, 1000).with {
                chunks << remainder
                value = div
            }
        }
        if (chunks.size() > big.size()) {
            throw new IllegalArgumentException("Number overflow")
        }
        def text = []
        (0..<chunks.size()).each { index ->
            if (chunks[index] > 0) {
                text << "${toText(chunks[index])}${index == 0 ? '' : ' ' + big[index]}"
                if (index == 0 && chunks[index] < 100) {
                    text << "and"
                }
            }
        }
        text.reverse().join(', ').replace(', and,', ' and')
    }
}

// Add this method to all Numbers
Number.metaClass.toText = { toText(delegate) }

println toText(29)
println 40.toText()
println toText(401)
println 9003.toText()
println toText(8011673)
println 8000100.toText()
println 4629436.toText()
948623487512387455323784623842314234.toText().split(',').each { println it.trim() }

def verifyToText(expected, value) {
    println "Checking '$expected' == $value"
    def actual = value.toText()
    assert expected == actual
}

verifyToText 'nineteen', 19
verifyToText 'one thousand, two hundred and thirty four', 1234
verifyToText 'twenty three million, four hundred and fifty nine thousand, six hundred and twelve', 23459612
verifyToText 'one thousand, nine hundred and ninety nine', 1999
verifyToText 'negative six hundred and one', -601
verifyToText 'twelve billion and nineteen', 12000000019
verifyToText 'negative one billion, two hundred and thirty four million, five hundred and sixty seven thousand, eight hundred and ninety', -1234567890
verifyToText 'one hundred and one', 101
verifyToText 'one thousand and one', 1001
verifyToText 'one million, one hundred and one', 1000101
verifyToText 'one million and forty five', 1000045
verifyToText 'one million and fifteen', 1000015
verifyToText 'one billion, forty five thousand and one', 1000045001

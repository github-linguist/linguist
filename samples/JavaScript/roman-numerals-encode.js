var roman = {
    map: [
        1000, 'M', 900, 'CM', 500, 'D', 400, 'CD', 100, 'C', 90, 'XC',
        50, 'L', 40, 'XL', 10, 'X', 9, 'IX', 5, 'V', 4, 'IV', 1, 'I',
    ],
    int_to_roman: function(n) {
        var value = '';
        for (var idx = 0; n > 0 && idx < this.map.length; idx += 2) {
            while (n >= this.map[idx]) {
                value += this.map[idx + 1];
                n -= this.map[idx];
            }
        }
        return value;
    }
}

roman.int_to_roman(1999); // "MCMXCIX"

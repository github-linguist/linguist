function levenshtein(str1, str2) {
    var m = str1.length,
        n = str2.length,
        d = [],
        i, j;
	
    if (!m) return n;
    if (!n) return m;
	
    for (i = 0; i <= m; i++) d[i] = [i];
    for (j = 0; j <= n; j++) d[0][j] = j;
	
    for (j = 1; j <= n; j++) {
        for (i = 1; i <= m; i++) {
            if (str1[i-1] == str2[j-1]) d[i][j] = d[i - 1][j - 1];
            else d[i][j] = Math.min(d[i-1][j], d[i][j-1], d[i-1][j-1]) + 1;
        }
    }
    return d[m][n];
}

console.log(levenshtein("kitten", "sitting"));
console.log(levenshtein("stop", "tops"));
console.log(levenshtein("rosettacode", "raisethysword"));

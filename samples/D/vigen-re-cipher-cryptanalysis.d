import std.stdio, std.algorithm, std.typecons, std.string,
       std.array, std.numeric, std.ascii;

string[2] vigenereDecrypt(in double[] targetFreqs, in string input) {
    enum nAlpha = std.ascii.uppercase.length;

    static double correlation(in string txt, in double[] sTargets)
    /*pure nothrow*/ {
        uint[nAlpha] charCounts = 0;
        foreach (immutable c; txt)
            charCounts[c - 'A']++;
        return charCounts[].sort().release.dotProduct(sTargets);
    }

    static frequency(in string txt) pure nothrow {
        auto freqs = new Tuple!(char,"c", uint,"d")[nAlpha];
        foreach (immutable i, immutable c; std.ascii.uppercase)
            freqs[i] = tuple(c, 0);
        foreach (immutable c; txt)
            freqs[c - 'A'].d++;
        return freqs;
    }

    static string[2] decode(in string cleaned, in string key)
    pure nothrow {
        assert(key.length > 0);
        string decoded;
        foreach (immutable i, immutable c; cleaned)
            decoded ~= (c - key[i % $] + nAlpha) % nAlpha + 'A';
        return [key, decoded];
    }

    static size_t findBestLength(in string cleaned,
                                 in double[] sTargets)
    /*pure nothrow*/ {
        size_t bestLength;
        double bestCorr = -100.0;

        // Assume that if there are less than 20 characters
        // per column, the key's too long to guess
        foreach (immutable i; 2 .. cleaned.length / 20) {
            auto pieces = new Appender!string[i];
            foreach (immutable j, immutable c; cleaned)
                pieces[j % i] ~= c;

            // The correlation seems to increase for smaller
            // pieces/longer keys, so weigh against them a little
            double corr = -0.5 * i;
            foreach (const p; pieces)
                corr += correlation(p.data, sTargets);

            if (corr > bestCorr) {
                bestLength = i;
                bestCorr = corr;
            }
        }

        return bestLength;
    }

    static string findKey(in string cleaned, in size_t bestLength,
                          in double[] targetFreqs) /*pure*/ {
        auto pieces = new string[bestLength];
        foreach (immutable i, immutable c; cleaned)
            pieces[i % bestLength] ~= c;

        string key;
        foreach (fr; pieces.map!frequency) {
            fr.sort!q{ a.d > b.d };

            size_t m;
            double maxCorr = 0.0;
            foreach (immutable j, immutable c; uppercase) {
                double corr = 0.0;
                foreach (immutable frc; fr) {
                    immutable di = (frc.c - c + nAlpha) % nAlpha;
                    corr += frc.d * targetFreqs[di];
                }

                if (corr > maxCorr) {
                    m = j;
                    maxCorr = corr;
                }
            }

            key ~= m + 'A';
        }

        return key;
    }

    immutable cleaned = input.toUpper.removechars("^A-Z");

    //immutable sortedTargets = sorted(targetFreqs);
    immutable sortedTargets = targetFreqs.dup.sort().release.idup;

    immutable bestLength = findBestLength(cleaned, sortedTargets);
    if (bestLength == 0)
        throw new Exception("Text is too short to analyze.");

    immutable string key = findKey(cleaned, bestLength, targetFreqs);
    return decode(cleaned, key);
}


void main() {
    immutable encoded = "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG
JSPXY ALUYM NSMYH VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF
WHTCQ KMLRD ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA
LWQIS FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG
ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ ILOVV
RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS JLAKI FHXUF
XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT LPRWM JAZPK LQUZA
ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST MTEOE PAPJH SMFNB YVQUZ
AALGA YDNMP AQOWT UHDBV TSMUE UIMVH QGVRW AEFSP EMPVE PKXZY WLKJA
GWALT VYYOB YIXOK IHPDS EVLEV RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY
IMAPX UOISK PVAGN MZHPW TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV
YOVDJ SOLXG TGRVO SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV
GJOKM SIFPR ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO
ZQDLX BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB
BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA FWAML
ZZRXJ EKAHV FASMU LVVUT TGK";

    immutable englishFrequences = [0.08167, 0.01492, 0.02782, 0.04253,
        0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772,
        0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987,
        0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974,
        0.00074];

    immutable key_dec = vigenereDecrypt(englishFrequences, encoded);
    writefln("Key: %s\n\nText: %s", key_dec[0], key_dec[1]);
}

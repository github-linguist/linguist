#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

const char *encoded =
    "MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH"
    "VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD"
    "ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS"
    "FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG"
    "ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ"
    "ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS"
    "JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT"
    "LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST"
    "MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH"
    "QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV"
    "RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW"
    "TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO"
    "SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR"
    "ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX"
    "BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB"
    "BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA"
    "FWAML ZZRXJ EKAHV FASMU LVVUT TGK";

const double freq[] = {
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
    0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
    0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
    0.00978, 0.02360, 0.00150, 0.01974, 0.00074
};

int best_match(const double *a, const double *b) {
    double sum = 0, fit, d, best_fit = 1e100;
    int i, rotate, best_rotate = 0;
    for (i = 0; i < 26; i++)
        sum += a[i];
    for (rotate = 0; rotate < 26; rotate++) {
        fit = 0;
        for (i = 0; i < 26; i++) {
            d = a[(i + rotate) % 26] / sum - b[i];
            fit += d * d / b[i];
        }

        if (fit < best_fit) {
            best_fit = fit;
            best_rotate = rotate;
        }
    }

    return best_rotate;
}

double freq_every_nth(const int *msg, int len, int interval, char *key) {
    double sum, d, ret;
    double out[26], accu[26] = {0};
    int i, j, rot;

    for (j = 0; j < interval; j++) {
        for (i = 0; i < 26; i++)
            out[i] = 0;
        for (i = j; i < len; i += interval)
            out[msg[i]]++;
        key[j] = rot = best_match(out, freq);
        key[j] += 'A';
        for (i = 0; i < 26; i++)
            accu[i] += out[(i + rot) % 26];
    }

    for (i = 0, sum = 0; i < 26; i++)
        sum += accu[i];

    for (i = 0, ret = 0; i < 26; i++) {
        d = accu[i] / sum - freq[i];
        ret += d * d / freq[i];
    }

    key[interval] = '\0';
    return ret;
}

int main() {
    int txt[strlen(encoded)];
    int len = 0, j;
    char key[100];
    double fit, best_fit = 1e100;

    for (j = 0; encoded[j] != '\0'; j++)
        if (isupper(encoded[j]))
            txt[len++] = encoded[j] - 'A';

    for (j = 1; j < 30; j++) {
        fit = freq_every_nth(txt, len, j, key);
        printf("%f, key length: %2d, %s", fit, j, key);
        if (fit < best_fit) {
            best_fit = fit;
            printf(" <--- best so far");
        }
        printf("\n");
    }

    return 0;
}

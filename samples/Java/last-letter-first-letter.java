// derived from C
final class LastLetterFirstLetter {
    static int maxPathLength = 0;
    static int maxPathLengthCount = 0;
    static final StringBuffer maxPathExample = new StringBuffer(500);

    static final String[] names = {"audino", "bagon", "baltoy", "banette",
        "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
        "cresselia", "croagunk", "darmanitan", "deino", "emboar",
        "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
        "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
        "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
        "loudred", "lumineon", "lunatone", "machamp", "magnezone",
        "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
        "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
        "registeel", "relicanth", "remoraid", "rufflet", "sableye",
        "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
        "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
        "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
        "wailord", "wartortle", "whismur", "wingull", "yamask"};

    static void recursive(String[] part, int offset) {
        if (offset > maxPathLength) {
            maxPathLength = offset;
            maxPathLengthCount = 1;
        } else if (offset == maxPathLength) {
            maxPathLengthCount++;
            maxPathExample.setLength(0);
            for (int i = 0; i < offset; i++) {
                maxPathExample.append((i % 5 == 0 ? "\n  " : " "));
                maxPathExample.append(part[i]);
            }
        }
        final char lastChar = part[offset - 1].charAt(part[offset - 1].length()-1);
        for (int i = offset; i < part.length; i++) {
            if (part[i].charAt(0) == lastChar) {
                String tmp = names[offset];
                names[offset] = names[i];
                names[i] = tmp;
                recursive(names, offset+1);
                names[i] = names[offset];
                names[offset] = tmp;
            }
        }
    }

    public static void main(String[] args) {
        for (int i = 0; i < names.length; i++) {
            String tmp = names[0];
            names[0] = names[i];
            names[i] = tmp;
            recursive(names, 1);
            names[i] = names[0];
            names[0] = tmp;
       }
       System.out.println("maximum path length        : " + maxPathLength);
       System.out.println("paths of that length       : " + maxPathLengthCount);
       System.out.println("example path of that length:" + maxPathExample);
    }
}

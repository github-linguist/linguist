class StripChars {
    public static String stripChars(String inString, String toStrip) {
        return inString.replaceAll("[" + toStrip + "]", "");
    }

    public static void main(String[] args) {
        String sentence = "She was a soul stripper. She took my heart!";
        String chars = "aei";
        System.out.println("sentence: " + sentence);
        System.out.println("to strip: " + chars);
        System.out.println("stripped: " + stripChars(sentence, chars));
    }
}

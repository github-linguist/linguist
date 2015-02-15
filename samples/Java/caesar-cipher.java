public class Cipher {
	public static void main(String[] args) {
		String enc = Cipher.encode(
				"The quick brown fox Jumped over the lazy Dog", 12);
		System.out.println(enc);
		System.out.println(Cipher.decode(enc, 12));
	}

	public static String decode(String enc, int offset) {
		return encode(enc, -offset);
	}

	public static String encode(String enc, int offset) {
		offset = offset % 26 + 26;
		StringBuilder encoded = new StringBuilder();
		for (char i : enc.toLowerCase().toCharArray()) {
			if (Character.isLetter(i)) {
				int j = (i - 'a' + offset) % 26;
				encoded.append((char) (j + 'a'));
			} else {
				encoded.append(i);
			}
		}
		return encoded.toString();
	}
}

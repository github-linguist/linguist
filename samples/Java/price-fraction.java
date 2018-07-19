import java.util.Random;

public class Main {
	private static float priceFraction(float f) {
		if (0.00f <= f && f < 0.06f) return 0.10f;
		else if (f < 0.11f) return 0.18f;
		else if (f < 0.16f) return 0.26f;
		else if (f < 0.21f) return 0.32f;
		else if (f < 0.26f) return 0.38f;
		else if (f < 0.31f) return 0.44f;
		else if (f < 0.36f) return 0.50f;
		else if (f < 0.41f) return 0.54f;
		else if (f < 0.46f) return 0.58f;
		else if (f < 0.51f) return 0.62f;
		else if (f < 0.56f) return 0.66f;
		else if (f < 0.61f) return 0.70f;
		else if (f < 0.66f) return 0.74f;
		else if (f < 0.71f) return 0.78f;
		else if (f < 0.76f) return 0.82f;
		else if (f < 0.81f) return 0.86f;
		else if (f < 0.86f) return 0.90f;
		else if (f < 0.91f) return 0.94f;
		else if (f < 0.96f) return 0.98f;
		else if (f < 1.01f) return 1.00f;
		else throw new IllegalArgumentException();
	}

	public static void main(String[] args) {
		Random rnd = new Random();
		for (int i = 0; i < 5; i++) {
			float f = rnd.nextFloat();
			System.out.format("%8.6f -> %4.2f%n", f, priceFraction(f));
		}
	}
}

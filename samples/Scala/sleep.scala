import java.util.Scanner

object Sleeper extends Application {
		val input = new Scanner(System.in)
		val ms = input.nextInt
		System.out.println("Sleeping...")
		Thread.sleep(ms)
		System.out.println("Awake!")
}

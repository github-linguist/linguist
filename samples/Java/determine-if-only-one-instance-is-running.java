import java.io.IOExeception;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;

public class SingletonApp
{
	private static final int PORT = 12345;		// random large port number
	private static ServerSocket s;

	// static initializer
	{
		try {
			s = new ServerSocket(PORT, 10, InetAddress.getLocalHost());
		} catch (UnknownHostException e) {
			// shouldn't happen for localhost
		} catch (IOException e) {
			// port taken, so app is already running
			System.exit(0);
		}
	}
	// main() and rest of application...
}

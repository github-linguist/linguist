import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class EchoServer {
	ServerSocket serverSocket;	
	public EchoServer(){		
	}
	
	public void start() {		
		try {
			serverSocket = new ServerSocket(12321);
			while(true){
				Thread clientThread = new Thread(new ClientHandler(serverSocket.accept()));
				clientThread.start();
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				System.out.println("closing server socket");
				serverSocket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
	}
	
	public static void main(String[] args) {
		EchoServer es = new EchoServer();
		es.start();
	}
}

class ClientHandler implements Runnable {
	private static int numConnections;
	private int connectionId = 0;
	Socket clientSocket;
	
	public ClientHandler(Socket s) {
		connectionId = numConnections++;
		System.out.println("handling connection, #" + connectionId);
		clientSocket = s;
	}

	public void run() {
		PrintWriter out = null;
		BufferedReader in = null;
		try {
			out = new PrintWriter(clientSocket.getOutputStream(), true);
			in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
			String inputLine, outputLine;
			while((inputLine = in.readLine()) != null){
				outputLine = inputLine;
				System.out.println("recieved: " + outputLine);
				out.write(outputLine+"\n");
				out.flush();
				if (outputLine.equals("exit"))
					break;
			}
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			out.close();
			try {
				in.close();
				clientSocket.close();
				System.out.println("closing connection, #" + connectionId);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}

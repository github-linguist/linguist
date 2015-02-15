import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class HelloWorld{
  public static void main(String[] args) throws IOException{
    ServerSocket listener = new ServerSocket(8080);
    while(true){
      Socket sock = listener.accept();
      new PrintWriter(sock.getOutputStream(), true).
                println("Goodbye, World!");
      sock.close();
    }
  }
}

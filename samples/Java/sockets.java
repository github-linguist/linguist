import java.io.IOException;
import java.net.*;
public class SocketSend {
  public static void main(String args[]) throws IOException {
    sendData("localhost", "hello socket world");
  }

  public static void sendData(String host, String msg) throws IOException {
    Socket sock = new Socket( host, 256 );
    sock.getOutputStream().write(msg.getBytes());
    sock.getOutputStream().flush();
    sock.close();
  }
}

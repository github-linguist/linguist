import java.net.*;
class DiscoverHostName {
 public static void main(final String[] args) {
  try {
   System.out.println(InetAddress.getLocalHost().getHostName());
  } catch (UnknownHostException e) { // Doesn't actually happen, but Java requires it be handled.
  }
 }
}

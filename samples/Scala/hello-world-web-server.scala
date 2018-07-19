import java.io.PrintWriter
import java.net.ServerSocket

object HelloWorld extends App {

  val text =
    <HTML>
      <HEAD>
        <TITLE>Hello world </TITLE>
      </HEAD>
      <BODY LANG="en-US" BGCOLOR="#e6e6ff" DIR="LTR">
        <P ALIGN="CENTER"> <FONT FACE="Arial, sans-serif" SIZE="6">Goodbye, World!</FONT> </P>
      </BODY>
    </HTML>
val port = 8080
  val listener = new ServerSocket(port)
   printf("Listening at port %1$d", port)

  while (true) {
    val sock = listener.accept()
    new PrintWriter(sock.getOutputStream(), true).println(text)
    sock.close()
  }
}

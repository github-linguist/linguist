using inet

class Socket
{
  public static Void main ()
  {
    sock := TcpSocket()
    sock.connect(IpAddr("localhost"), 256)

    sock.out.printLine("hello socket world")
    sock.out.flush
    sock.close
  }
}

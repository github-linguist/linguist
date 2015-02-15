import java.io.*;
import java.net.*;
import java.util.*;

public class ChatServer implements Runnable
{
  private int port = 0;
  private List<Client> clients = new ArrayList<Client>();

  public ChatServer(int port)
  {  this.port = port;  }

  public void run()
  {
    try
    {
      ServerSocket ss = new ServerSocket(port);
      while (true)
      {
        Socket s = ss.accept();
        new Thread(new Client(s)).start();
      }
    }
    catch (Exception e)
    {  e.printStackTrace();  }
  }

  private synchronized boolean registerClient(Client client)
  {
    for (Client otherClient : clients)
      if (otherClient.clientName.equalsIgnoreCase(client.clientName))
        return false;
    clients.add(client);
    return true;
  }

  private void deregisterClient(Client client)
  {
    boolean wasRegistered = false;
    synchronized (this)
    {  wasRegistered = clients.remove(client);  }
    if (wasRegistered)
      broadcast(client, "--- " + client.clientName + " left ---");
  }

  private synchronized String getOnlineListCSV()
  {
    StringBuilder sb = new StringBuilder();
    sb.append(clients.size()).append(" user(s) online: ");
    for (int i = 0; i < clients.size(); i++)
      sb.append((i > 0) ? ", " : "").append(clients.get(i).clientName);
    return sb.toString();
  }

  private void broadcast(Client fromClient, String msg)
  {
    // Copy client list (don't want to hold lock while doing IO)
    List<Client> clients = null;
    synchronized (this)
    {  clients = new ArrayList<Client>(this.clients);  }
    for (Client client : clients)
    {
      if (client.equals(fromClient))
        continue;
      try
      {  client.write(msg + "\r\n");  }
      catch (Exception e)
      {  }
    }
  }

  public class Client implements Runnable
  {
    private Socket socket = null;
    private Writer output = null;
    private String clientName = null;

    public Client(Socket socket)
    {
      this.socket = socket;
    }

    public void run()
    {
      try
      {
        socket.setSendBufferSize(16384);
        socket.setTcpNoDelay(true);
        BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        output = new OutputStreamWriter(socket.getOutputStream());
        write("Please enter your name: ");
        String line = null;
        while ((line = input.readLine()) != null)
        {
          if (clientName == null)
          {
            line = line.trim();
            if (line.isEmpty())
            {
              write("A name is required. Please enter your name: ");
              continue;
            }
            clientName = line;
            if (!registerClient(this))
            {
              clientName = null;
              write("Name already registered. Please enter your name: ");
              continue;
            }
            write(getOnlineListCSV() + "\r\n");
            broadcast(this, "+++ " + clientName + " arrived +++");
            continue;
          }
          if (line.equalsIgnoreCase("/quit"))
            return;
          broadcast(this, clientName + "> " + line);
        }
      }
      catch (Exception e)
      {  }
      finally
      {
        deregisterClient(this);
        output = null;
        try
        {  socket.close();  }
        catch (Exception e)
        {  }
        socket = null;
      }
    }

    public void write(String msg) throws IOException
    {
      output.write(msg);
      output.flush();
    }

    public boolean equals(Client client)
    {
      return (client != null) && (client instanceof Client) && (clientName != null) && (client.clientName != null) && clientName.equals(client.clientName);
    }
  }

  public static void main(String[] args)
  {
    int port = 4004;
    if (args.length > 0)
      port = Integer.parseInt(args[0]);
    new ChatServer(port).run();
  }
}

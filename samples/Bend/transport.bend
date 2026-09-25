import Base
import ./json.bend as J

type Connection is Type:
  Connection{socket: Socket, buffered: String}

type Read is Type:
  Frame{connection: Connection, value: J.Json}
  Broken{message: String}

type Cut is Data:
  Complete{line: String, rest: String}
  Partial{}

def cut(s: String, acc: String) -> Cut:
  match s:
    case SNil{}: Partial{}
    case SCon{'\n', rest}: Complete{String.reverse(acc), rest}
    case SCon{c, rest}: cut(rest, SCon{c, acc})

def broken(socket: Socket, message: String) -> IO(Read):
  do IO<Read>:
    Socket.close(socket)
    return Broken{message}

def decoded(result: Result<&1, &1, String, J.Json>, socket: Socket, rest: String) -> IO(Read):
  match result:
    case Fail{error}: broken(socket, "Invalid bridge JSON: " ++ error)
    case Done{value}: IO.pure(Read, Frame{Connection{socket, rest}, value})

def chunk_nonempty(empty: Bool, socket: Socket, chunk: String, buffered: String,
                   next: Connection -> IO(Read)) -> IO(Read):
  match empty:
    case True{}: broken(socket, "Bridge closed before a complete frame")
    case False{}: next(Connection{socket, buffered ++ chunk})

def chunk(result: Socket & Result<&1, &1, U32 & String, String>, buffered: String,
          next: Connection -> IO(Read)) -> IO(Read):
  (socket, received) = result
  match received:
    case Fail{(code, message)}: broken(socket, "Bridge receive failed: " ++ message)
    case Done{+data}: chunk_nonempty(String.is_empty(data), socket, data, buffered, next)

def read_cut(part: Cut, socket: Socket, buffered: String,
             next: Connection -> IO(Read)) -> IO(Read):
  match part:
    case Complete{line, rest}: decoded(J.read(line), socket, rest)
    case Partial{}:
      do IO<Read>:
        received : Socket & Result<&1, &1, U32 & String, String> <- TCP.recv(socket, 4096)
        chunk(received, buffered, next)

def read_limit(large: Bool, socket: Socket, +buffered: String,
               next: Connection -> IO(Read)) -> IO(Read):
  match large:
    case True{}: broken(socket, "Bridge frame exceeds 4 MiB limit")
    case False{}: read_cut(cut(buffered, ""), socket, buffered, next)

# Socket progress depends on the external peer, not structural recursion.
@unsafe
def read(connection: Connection) -> IO(Read):
  match connection:
    case Connection{socket, +buffered}:
      read_limit((U32.from_nat(String.length(buffered)) > 4198400 : U32), socket, buffered, c => read(c))

def sent(result: Socket & Result<&1, &1, U32 & String, Unit>) -> IO(Result<&1, &1, String, Connection>):
  (socket, result) = result
  match result:
    case Fail{(code, message)}:
      do IO<Result<&1, &1, String, Connection>>:
        Socket.close(socket)
        return Fail{message}
    case Done{unit}: IO.pure(Result<&1, &1, String, Connection>, Done{Connection{socket, ""}})

def connected(result: Result<&1, &1, U32 & String, Socket>, body: String) -> IO(Result<&1, &1, String, Connection>):
  match result:
    case Fail{(code, message)}: IO.pure(Result<&1, &1, String, Connection>, Fail{message})
    case Done{socket}:
      do IO<Result<&1, &1, String, Connection>>:
        result : Socket & Result<&1, &1, U32 & String, Unit> <- TCP.send(socket, body ++ "\n")
        sent(result)

def connect(port: U32, body: String) -> IO(Result<&1, &1, String, Connection>):
  do IO<Result<&1, &1, String, Connection>>:
    result : Result<&1, &1, U32 & String, Socket> <- TCP.connect("127.0.0.1", port)
    connected(result, body)

def close(connection: Connection) -> IO(Unit):
  match connection:
    case Connection{socket, buffered}: Socket.close(socket)

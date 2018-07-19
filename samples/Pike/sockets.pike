import Stdio;

int main(){
   object con = File();
   con->connect("127.0.0.1",256);
   con->write("hello socket world");
   con->close();
}

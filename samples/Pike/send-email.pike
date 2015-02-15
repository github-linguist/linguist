int main(){
   string to         = "some@email.add";
   string subject    = "Hello There.";
   string from       = "me@myaddr.ess";
   string msg        = "Hello there! :)";

   Protocols.SMTP.Client()->simple_mail(to,subject,from,msg);
}

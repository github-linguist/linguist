using web
using wisp

const class HelloMod : WebMod // provides the content
{
  override Void onGet ()
  {
    res.headers["Content-Type"] = "text/plain; charset=utf-8"
    res.out.print ("Goodbye, World!")
  }
}

class HelloWeb
{
  Void main ()
  {
    WispService // creates the web service
    {
      port = 8080
      root = HelloMod()
    }.start

    while (true) {} // stay running
  }
}

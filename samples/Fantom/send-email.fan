using email

class Mail
{
  // create a client for sending email - add your own host/username/password
  static SmtpClient makeClient ()
  {
    client := SmtpClient
    {
      host     = "yourhost"
      username = "yourusername"
      password = "yourpassword"
    }
    return client
  }

  public static Void main()
  {
    // create email
    email := Email
    {
      to = ["to@addr"]
      from = "from@addr"
      cc = ["cc@addr"]
      subject = test"
      body = TextPart { text = "test email" }
    }

    // create client and send email
    makeClient.send (email)
  }
}

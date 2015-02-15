// on Ubuntu: sudo apt-get install libpoco-dev
// or see http://pocoproject.org/
// compile with: g++ -Wall -O3 send-mail-cxx.C -lPocoNet -lPocoFoundation

#include <cstdlib>
#include <iostream>
#include <Poco/Net/SMTPClientSession.h>
#include <Poco/Net/MailMessage.h>

using namespace Poco::Net;

int main (int argc, char **argv)
{
  try
    {
      MailMessage msg;

      msg.addRecipient (MailRecipient (MailRecipient::PRIMARY_RECIPIENT,
                                       "alice@example.com",
                                       "Alice Moralis"));
      msg.addRecipient (MailRecipient (MailRecipient::CC_RECIPIENT,
                                       "pat@example.com",
                                       "Patrick Kilpatrick"));
      msg.addRecipient (MailRecipient (MailRecipient::BCC_RECIPIENT,
                                       "mike@example.com",
                                       "Michael Carmichael"));

      msg.setSender ("Roy Kilroy <roy@example.com>");

      msg.setSubject ("Rosetta Code");
      msg.setContent ("Sending mail from C++ using POCO C++ Libraries");

      SMTPClientSession smtp ("mail.example.com"); // SMTP server name
      smtp.login ();
      smtp.sendMessage (msg);
      smtp.close ();
      std::cerr << "Sent mail successfully!" << std::endl;
    }
  catch (std::exception &e)
    {
      std::cerr << "failed to send mail: " << e.what() << std::endl;
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

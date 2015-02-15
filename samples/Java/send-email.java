import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.Message.RecipientType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

/**
 * Mail
 */
public class Mail
{
 /**
  * Session
  */
 protected Session session;

 /**
  * Mail constructor.
  *
  * @param host Host
  */
 public Mail(String host)
 {
  Properties properties = new Properties();
  properties.put("mail.smtp.host", host);
  session = Session.getDefaultInstance(properties);
 }

 /**
  * Send email message.
  *
  * @param from From
  * @param tos Recipients
  * @param ccs CC Recipients
  * @param subject Subject
  * @param text Text
  * @throws MessagingException
  */
 public void send(String from, String tos[], String ccs[], String subject,
        String text)
        throws MessagingException
 {
  MimeMessage message = new MimeMessage(session);
  message.setFrom(new InternetAddress(from));
  for (String to : tos)
   message.addRecipient(RecipientType.TO, new InternetAddress(to));
  for (String cc : ccs)
   message.addRecipient(RecipientType.TO, new InternetAddress(cc));
  message.setSubject(subject);
  message.setText(text);
  Transport.send(message);
 }
}

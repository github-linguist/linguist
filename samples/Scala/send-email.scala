import java.util.Properties

import javax.mail.internet.{ InternetAddress, MimeMessage }
import javax.mail.Message.RecipientType
import javax.mail.{ Session, Transport }

/** Mail constructor.
 *  @constructor Mail
 *  @param host Host
 */
class Mail(host: String) {
  val session = Session.getDefaultInstance(new Properties() { put("mail.smtp.host", host) })

  /** Send email message.
   *
   *  @param from From
   *  @param tos Recipients
   *  @param ccs CC Recipients
   *  @param subject Subject
   *  @param text Text
   *  @throws MessagingException
   */
  def send(from: String, tos: List[String], ccs: List[String], subject: String, text: String) {
    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(from))
    for (to <- tos)
      message.addRecipient(RecipientType.TO, new InternetAddress(to))
    for (cc <- ccs)
      message.addRecipient(RecipientType.TO, new InternetAddress(cc))
    message.setSubject(subject)
    message.setText(text)
    Transport.send(message)
  }
}

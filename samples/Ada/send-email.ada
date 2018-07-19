with AWS.SMTP, AWS.SMTP.Client, AWS.SMTP.Authentication.Plain;
with Ada.Text_IO;
use  Ada, AWS;

procedure Sendmail is
   Status : SMTP.Status;
   Auth : aliased constant SMTP.Authentication.Plain.Credential :=
      SMTP.Authentication.Plain.Initialize ("id", "password");
   Isp : SMTP.Receiver;
begin
   Isp :=
      SMTP.Client.Initialize
        ("smtp.mail.com",
         Port       => 5025,
         Credential => Auth'Unchecked_Access);
   SMTP.Client.Send
     (Isp,
      From    => SMTP.E_Mail ("Me", "me@some.org"),
      To      => SMTP.E_Mail ("You", "you@any.org"),
      Subject => "subject",
      Message => "Here is the text",
      Status  => Status);
   if not SMTP.Is_Ok (Status) then
      Text_IO.Put_Line
        ("Can't send message :" & SMTP.Status_Message (Status));
   end if;
end Sendmail;

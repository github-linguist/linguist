let h = Smtp.connect "smtp.gmail.fr";;
Smtp.helo h "hostname";;
Smtp.mail h "<john.smith@example.com>";;
Smtp.rcpt h "<john-doe@example.com>";;
let email_header = "\
From: John Smith <john.smith@example.com>
To: John Doe <john-doe@example.com>
Subject: surprise";;
let email_msg = "Happy Birthday";;
Smtp.data h (email_header ^ "\r\n\r\n" ^ email_msg);;
Smtp.quit h;;

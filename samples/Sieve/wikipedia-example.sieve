# This script was copied from https://en.wikipedia.org/wiki/Sieve_(mail_filtering_language)#Example
# on 2020-04-08.
# Sieve filter

# Declare the extensions used by this script.
#
require ["fileinto", "reject"];

# Messages bigger than 100K will be rejected with an error message
#
if size :over 100K {
   reject "I'm sorry, I do not accept mail over 100kb in size.
Please upload larger files to a server and send me a link.
Thanks.";
}

# Mails from a mailing list will be put into the folder "mailinglist"
#
elsif address :is ["From", "To"] "mailinglist@blafasel.invalid" {
   fileinto "INBOX.mailinglist";
}

# Spam Rule: Message does not contain my address in To, CC or Bcc
# header, or subject is something with "money" or "Viagra".
#
elsif anyof (not address :all :contains ["To", "Cc", "Bcc"] "me@blafasel.invalid",
header :matches "Subject" ["*money*","*Viagra*"]) {
      fileinto "INBOX.spam";
}

# Keep the rest.
# This is not necessary because there is a "implicit keep" Rule
#
else {
     keep;
}

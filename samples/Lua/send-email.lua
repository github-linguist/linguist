-- load the smtp support
local smtp = require("socket.smtp")

-- Connects to server "localhost" and sends a message to users
-- "fulano@example.com",  "beltrano@example.com",
-- and "sicrano@example.com".
-- Note that "fulano" is the primary recipient, "beltrano" receives a
-- carbon copy and neither of them knows that "sicrano" received a blind
-- carbon copy of the message.
from = "<luasocket@example.com>"

rcpt = {
  "<fulano@example.com>",
  "<beltrano@example.com>",
  "<sicrano@example.com>"
}

mesgt = {
  headers = {
    to = "Fulano da Silva <fulano@example.com>",
    cc = '"Beltrano F. Nunes" <beltrano@example.com>',
    subject = "My first message"
  },
  body = "I hope this works. If it does, I can send you another 1000 copies."
}

r, e = smtp.send{
  from = from,
  rcpt = rcpt,
  source = smtp.message(mesgt)
}

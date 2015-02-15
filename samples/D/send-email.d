import std.net.curl;

void main() {
    auto smtp = SMTP("smtps://smtp.gmail.com");
    smtp.setAuthentication("someuser@gmail.com", "somepassword");
    smtp.mailTo = ["<friend@example.com>"];
    smtp.mailFrom = "<someuser@gmail.com>";
    smtp.message = "Subject:test\n\nExample Message";
    smtp.perform();
}

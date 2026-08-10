# Testing 200 OK
GET https://example.org/order/435
HTTP 200

# Testing status code is in a 200-300 range
GET https://example.org/order/435
HTTP *
[Asserts]
status >= 200
status < 300

# XPath asserts can be used to check HTML content
GET https://example.org
HTTP 200
Content-Type: text/html; charset=UTF-8
[Asserts]
xpath "string(/html/head/title)" contains "Example" # Check title
xpath "count(//p)" == 2                             # Check the number of p
xpath "//p" count == 2                              # Similar assert for p
xpath "boolean(count(//h2))" == false               # Check there is no h2
xpath "//h2" not exists                             # Similar assert for h2
xpath "string(//div[1])" matches /Hello.*/

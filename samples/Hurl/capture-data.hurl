# Go home and capture token
GET https://example.org
HTTP 200
[Captures]
csrf_token: xpath "string(//meta[@name='_csrf_token']/@content)"


# Do login!
POST https://example.org/login
X-CSRF-TOKEN: {{csrf_token}}
[Form]
user: toto
password: 1234
HTTP 302

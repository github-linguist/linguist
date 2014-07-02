# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Mail sender sample using the Curl module
module curl_mail

import curl

var curl = new Curl
var mail_request = new CurlMailRequest(curl)

# Networks
var response = mail_request.set_outgoing_server("smtps://smtp.example.org:465", "user@example.org", "mypassword")
if response isa CurlResponseFailed then
	print "Error code : {response.error_code}"
	print "Error msg : {response.error_msg}"
end

# Headers
mail_request.from = "Billy Bob"
mail_request.to = ["user@example.org"]
mail_request.cc = ["bob@example.org"]
mail_request.bcc = null

var headers_body = new HeaderMap
headers_body["Content-Type:"] = "text/html; charset=\"UTF-8\""
headers_body["Content-Transfer-Encoding:"] = "quoted-printable"
mail_request.headers_body = headers_body

# Content
mail_request.body = "<h1>Here you can write HTML stuff.</h1>"
mail_request.subject = "Hello From My Nit Program"

# Others
mail_request.verbose = false

# Send mail
response = mail_request.execute
if response isa CurlResponseFailed then
	print "Error code : {response.error_code}"
	print "Error msg : {response.error_msg}"
else if response isa CurlMailResponseSuccess then
	print "Mail Sent"
else
	print "Unknown Curl Response type"
end

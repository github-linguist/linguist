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

# Sample of the Curl module.
module curl_http

import curl

# Small class to represent an Http Fetcher
class MyHttpFetcher
	super CurlCallbacks

	var curl: Curl
	var our_body: String = ""

	init(curl: Curl) do self.curl = curl

	# Release curl object
	fun destroy do self.curl.destroy

	# Header callback
	redef fun header_callback(line: String) do
		# We keep this callback silent for testing purposes
		#if not line.has_prefix("Date:") then print "Header_callback : {line}"
	end

	# Body callback
	redef fun body_callback(line: String) do self.our_body = "{self.our_body}{line}"

	# Stream callback - Cf : No one is registered
	redef fun stream_callback(buffer: String, size: Int, count: Int) do print "Stream_callback : {buffer} - {size} - {count}"
end


# Program
if args.length < 2 then
	print "Usage: curl_http <method wished [POST, GET, GET_FILE]> <target url>"
else
	var curl = new Curl
	var url = args[1]
	var request = new CurlHTTPRequest(url, curl)

	# HTTP Get Request
	if args[0] == "GET" then
		request.verbose = false
		var getResponse = request.execute

		if getResponse isa CurlResponseSuccess then
			print "Status code : {getResponse.status_code}"
			print "Body : {getResponse.body_str}"
		else if getResponse isa CurlResponseFailed then
			print "Error code : {getResponse.error_code}"
			print "Error msg : {getResponse.error_msg}"
		end

	# HTTP Post Request
	else if args[0] == "POST" then
		var myHttpFetcher = new MyHttpFetcher(curl)
		request.delegate = myHttpFetcher

		var postDatas = new HeaderMap
		postDatas["Bugs Bunny"] = "Daffy Duck"
		postDatas["Batman"] = "Robin likes special characters @#ùà!è§'(\"é&://,;<>∞~*"
		postDatas["Batman"] = "Yes you can set multiple identical keys, but APACHE will consider only once, the last one"
		request.datas = postDatas
		request.verbose = false
		var postResponse = request.execute

		print "Our body from the callback : {myHttpFetcher.our_body}"

		if postResponse isa CurlResponseSuccess then
			print "*** Answer ***"
			print "Status code : {postResponse.status_code}"
			print "Body should be empty, because we decided to manage callbacks : {postResponse.body_str.length}"
		else if postResponse isa CurlResponseFailed then
			print "Error code : {postResponse.error_code}"
			print "Error msg : {postResponse.error_msg}"
		end

	# HTTP Get to file Request
	else if args[0] == "GET_FILE" then
		var headers = new HeaderMap
		headers["Accept"] = "Moo"
		request.headers = headers
		request.verbose = false
		var downloadResponse = request.download_to_file(null)

		if downloadResponse isa CurlFileResponseSuccess then
			print "*** Answer ***"
			print "Status code : {downloadResponse.status_code}"
			print "Size downloaded : {downloadResponse.size_download}"
		else if downloadResponse isa CurlResponseFailed then
			print "Error code : {downloadResponse.error_code}"
			print "Error msg : {downloadResponse.error_msg}"
		end
	# Program logic
	else
		print "Usage : Method[POST, GET, GET_FILE]"
	end
end

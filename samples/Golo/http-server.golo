# Copyright 2012-2014 Institut National des Sciences Appliquées de Lyon (INSA-Lyon)
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

module samples.WebServer

import java.lang
import java.net.InetSocketAddress
import com.sun.net.httpserver
import com.sun.net.httpserver.HttpServer

function main = |args| {

  let server = HttpServer.create(InetSocketAddress("localhost", 8081), 0)
  
  server: createContext("/", |exchange| {
    let headers = exchange: getResponseHeaders()
    let response = StringBuilder():
      append("Requested URI: "):
      append(exchange: getRequestURI()):
      append("\n"):
      append("Current time: "):
      append(java.util.Date()):
      append("\n"):
      toString()
    headers: set("Content-Type", "text/plain")
    exchange: sendResponseHeaders(200, response: length())
    exchange: getResponseBody(): write(response: getBytes())
    exchange: close()
  })

  server: createContext("/shutdown", |exchange| {
    let response = "Ok, thanks, bye!"
    exchange: getResponseHeaders(): set("Content-Type", "text/plain")
    exchange: sendResponseHeaders(200, response: length())
    exchange: getResponseBody(): write(response: getBytes())
    exchange: close()
    server: stop(5)
  })

  server: start()
  println(">>> http://localhost:8081/")
}

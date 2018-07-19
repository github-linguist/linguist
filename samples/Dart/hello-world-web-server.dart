import 'dart:io';

void main(){
  HttpServer.bind('localhost', 8080)
            .then((HttpServer server) =>
                server.listen((HttpRequest request) =>
            	    request..response.write('Hello, world')
            		   ..response.close()));
}

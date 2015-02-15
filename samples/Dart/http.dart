import 'dart:io';
void main(){
  String url = 'http://rosettacode.org';
  HttpClient client = new HttpClient();
  client.getUrl(Uri.parse(url))
        .then((HttpClientRequest request)   => request.close())
        .then((HttpClientResponse response) => response.pipe(stdout));
}

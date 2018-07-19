def app(environ, start_response):
    start_response('200 OK', [])
    yield "Goodbye, World!"

if __name__ == '__main__':
    from wsgiref.simple_server import make_server
    server = make_server('127.0.0.1', 8080, app)
    server.serve_forever()

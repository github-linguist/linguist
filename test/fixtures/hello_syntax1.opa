/**
 * To compile & run on port 8080:
 * opa hello_syntax1.opa --
 */
server = Server.one_page_server(
   "Hello, world",
   -> (<h1>Hello, world</h1>)
)


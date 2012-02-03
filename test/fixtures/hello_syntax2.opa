/**
 * To compile & run on port 8080:
 * opa --parser js-like hello_syntax2.opa --
 */
Server.start(
   Server.http,
   {
     page: function() { <h1>Hello, world</h1> },
     title: "Hello, world"
   }
)

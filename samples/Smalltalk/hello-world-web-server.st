Smalltalk loadPackage:'stx:goodies/webServer'. "usually already loaded"
|myServer service|

myServer := HTTPServer startServerOnPort:8082.
service := HTTPPluggableActionService new.
service
    register:[:request |
        self halt: 'debugging'.
        request reply:'<HTML><BODY><H1>Hello World</H1></BODY></HTML>'
    ]
    as:'hello'.
service linkNames:#('/' ).
service registerServiceOn: myServer.
myServer start.

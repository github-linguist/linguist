USING: accessors io io.encodings.utf8 io.servers.connection
threads ;
IN: rosetta.echo

CONSTANT: echo-port 12321

: handle-client ( -- )
    [ write "\r\n" write flush ] each-line ;

: <echo-server> ( -- threaded-server )
    utf8 <threaded-server>
        "echo-server" >>name
        echo-port >>insecure
        [ handle-client ] >>handler ;

: start-echo-server ( -- threaded-server )
    <echo-server> [ start-server ] in-thread ;

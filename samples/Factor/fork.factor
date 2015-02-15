USING: unix unix.process ;

[ "Hello form child" print flush 0 _exit ] [ drop "Hi from parent" print flush ] with-fork

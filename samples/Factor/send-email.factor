USING: kernel accessors smtp io.sockets namespaces ;
IN: learn

: send-mail ( from to cc subject body -- )
    "smtp.gmail.com" 587 <inet> smtp-server set
    smtp-tls? on
    "noneofyourbuisness@gmail.com" "password" <plain-auth> smtp-auth set
    <email>
        swap >>from
        swap >>to
        swap >>cc
        swap >>subject
        swap >>body
    send-email ;

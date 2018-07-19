package require tdbc

proc connect_db {handleName dbname host user pass} {
    package require tdbc::mysql
    tdbc::mysql::connection create $handleName -user $user -passwd $pass \
        -host $host -database $dbname
    return $handleName
}

# A simple helper to keep code shorter
proc r64k {} {
    expr int(65536*rand())
}

proc create_user {handle user pass} {
    set salt [binary format ssssssss \
        [r64k] [r64k] [r64k] [r64k] [r64k] [r64k] [r64k] [r64k]]
    # Note that we are using named parameters below, :user :salt :pass
    # They are bound automatically to local variables with the same name
    $handle allrows {
        INSERT IGNORE INTO users (username, pass_salt, pass_md5)
            VALUES (:user, :salt, unhex(md5(concat(:salt, :pass))))
    }
    return   ;# Ignore the result of the allrows method
}

proc authenticate_user {handle user pass} {
    $handle foreach row {
        SELECT userid FROM users WHERE
            username=:user AND pass_md5=unhex(md5(concat(pass_salt, :pass)))
    } {
        return [dict get $row userid]
    }
    # Only get here if no rows selected
    error "authentication failed for user \"$user\""
}

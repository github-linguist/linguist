#lang racket
(require db file/md5)
(define-logger authentication)
(current-logger authentication-logger)

(define DB-HOST "localhost")
(define DB-USER "devel")
(define DB-PASS "devel")	
(define DB-NAME "test")	

(define (connect-db)
  (mysql-connect
   #:user DB-USER
   #:database DB-NAME
   #:password DB-PASS))

(define (salt+password->hash salt password #:hex-encode? (hex-encode? #f))
  (md5 (bytes-append salt password) hex-encode?))

(define (report-sql-error e)
  (log-authentication-error "Failed to create user:~s" (exn-message e))
  #f)

(define (create-user db username passwd)
  ; if user was successfully created, returns its ID else #f
  (define salt (list->bytes (for/list ((i (in-range 16))) (random 256))))
  (define hash (salt+password->hash salt passwd))
  (with-handlers ((exn:fail:sql? report-sql-error))
    (query db "INSERT INTO users (username, pass_salt, pass_md5) VALUES (?, ?, ?)"
           username salt hash)))

(define (authenticate-user db username password)
  (or
   (match (query-maybe-row db "SELECT pass_salt, pass_md5 FROM users WHERE username = ?" username)
     [#f #f]
     [(vector salt hash) (bytes=? hash (salt+password->hash salt password))])
   ; don't let the deviants know whether it's the username or password that's dodgy
   (error "the username, password combination does not exist in system")))

(module+ test
  (require rackunit)
  (define test-DB (connect-db))
  ; typically, you only do this the once (or risk upsetting your users bigtime!)
  ; call this just the once!
  (define (create-users-table db)
    (query-exec db "DROP TABLE IF EXISTS users")
    (query-exec db #<<EOS
CREATE TABLE users (
    userid INT PRIMARY KEY AUTO_INCREMENT,
    username VARCHAR(32) UNIQUE KEY NOT NULL,
    pass_salt tinyblob NOT NULL,
            -- a string of 16 random bytes
    pass_md5 tinyblob NOT NULL
            -- binary MD5 hash of pass_salt concatenated with the password
);
EOS
                ))
  (create-users-table test-DB)
  (create-user test-DB #"tim" #"shh! it's a secret!")
  ; ensure the user exists (for testing purposes)
  (check-match (query-list test-DB "SELECT userid FROM users WHERE username = 'tim'") (list _))
  ; (ah... but tim exists!!!)
  (check-false (create-user test-DB #"tim" #"tim's password"))
  (check-exn exn:fail? (λ () (authenticate-user test-DB #"tim" #"password")))
  (check-true (authenticate-user test-DB #"tim" #"shh! it's a secret!")))

#lang racket

(require ffi/unsafe ffi/unsafe/define)

(define-ffi-definer defldap (ffi-lib "libldap"))
(defldap ldap_init (_fun _string _int -> _pointer))
(defldap ldap_unbind (_fun _pointer -> _void))
(defldap ldap_simple_bind_s (_fun _pointer _string _string -> _int))
(defldap ldap_err2string (_fun _int -> _string))

(define name ...)
(define password ...)
(define ld (ldap_init "ldap.somewhere.com" 389))
(ldap_simple_bind_s ld name password)

(ldap_unbind ld)

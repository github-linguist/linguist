#lang at-exp racket

(require db)
(define postal (sqlite3-connect #:database "/tmp/postal.db" #:mode 'create))

(define (add! name street city state zip)
  (query-exec postal
    @~a{INSERT INTO addresses (name, street, city, state, zip)
        VALUES (?, ?, ?, ?, ?)}
    name street city state zip))

(unless (table-exists? postal "addresses")
  (query-exec postal
    @~a{CREATE TABLE addresses(
          id INTEGER PRIMARY KEY,
          name   TEXT NOT NULL,
          street TEXT NOT NULL,
          city   TEXT NOT NULL,
          state  TEXT NOT NULL,
          zip    TEXT NOT NULL)}))

(add! "FSF Inc."
      "51 Franklin St"
      "Boston"
      "MA"
      "02110-1301")
(add! "The White House"
      "1600 Pennsylvania Avenue NW"
      "Washington"
      "DC"
      "20500")
(add! "National Security Council"
      "1700 Pennsylvania Avenue NW"
      "Washington"
      "DC"
      "20500")

(printf "Addresses:\n")
(for ([r (query-rows postal "SELECT * FROM addresses")])
  (printf "  ~a.\n" (string-join (cdr (vector->list r)) ", ")))
(newline)

(printf "By State+ZIP:\n")
(for ([z (query-rows postal "SELECT * FROM addresses"
                     #:group #("state" "zip"))])
  (printf "  ~a, ~a:\n" (vector-ref z 0) (vector-ref z 1))
  (for ([r (vector-ref z 2)])
    (printf "    ~a.\n" (string-join (cdr (vector->list r)) ", "))))

(disconnect postal)

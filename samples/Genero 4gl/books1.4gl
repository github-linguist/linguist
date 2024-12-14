-- To test the sample:
--
-- $ sed -n 's/\#\# \(.*\)/\1/p' books1.4gl > library.sch
-- $ fglform -M ../"Genero Forms"/books1.per
-- $ mv ../"Genero Forms"/books1.42f .
-- $ fglcomp -M books1.4gl
-- $ fglrun books1.42m
--

SCHEMA library

PUBLIC TYPE t_book RECORD LIKE book.*
PUBLIC TYPE t_book_array DYNAMIC ARRAY OF t_book

PRIVATE CONSTANT ID_UNKNOWN = 100

MAIN

    DEFINE books t_book_array
    DEFINE brec t_book
    DEFINE f_plot LIKE book.b_plot

    CONNECT TO ":memory:+driver='dbmsqt'"

    CALL create_tables()

    OPEN FORM f1 FROM "books1"
    DISPLAY FORM f1

    CALL fill_book_array(books)

    DIALOG ATTRIBUTES(UNBUFFERED)

    DISPLAY ARRAY books TO sr1.*

        BEFORE ROW
            LET f_plot = books[ arr_curr() ].b_plot

        ON ACTION nbrows ATTRIBUTES(TEXT="Row count")
            MESSAGE SFMT("Number of books: %1",books.getLength())

        ON INSERT
            LET f_plot = NULL
            INITIALIZE brec.* TO NULL
            LET brec.b_author = ID_UNKNOWN
            LET brec.b_pub_date = TODAY
            LET brec.b_price = 0
            CLEAR formonly.f_plot
            INPUT brec.* FROM sr1[ scr_line() ].*
                  ATTRIBUTES(WITHOUT DEFAULTS);
            IF NOT int_flag THEN
                TRY
                    INSERT INTO book VALUES brec.*
                    LET brec.book_id = sqlca.sqlerrd[2]
                    LET books[ DIALOG.getCurrentRow("sr1") ] = brec
                    MESSAGE SFMT("New book record created (id=%1)",brec.book_id)
                CATCH
                    ERROR "Could not insert new book row into database:", SQLERRMESSAGE
                END TRY
            END IF

        ON DELETE
            IF mbox_yn("Delete the current row?") THEN
                TRY
                    DELETE FROM book WHERE book_id = books[arr_curr()].book_id
                    MESSAGE "Book record was deleted"
                CATCH
                    ERROR "Could not delete the book row from database:", SQLERRMESSAGE
                END TRY
            END IF

    END DISPLAY

    INPUT BY NAME f_plot

        ON ACTION clear_plot ATTRIBUTES(TEXT="Clear")
            IF NOT mbox_yn("Are you sure you want to clear the plot summary?") THEN
                CONTINUE DIALOG
            END IF
            LET f_plot = NULL
            LET books[ arr_curr() ].b_plot = NULL
            UPDATE book SET b_plot = NULL
             WHERE book_id = books[ arr_curr() ].book_id

        ON ACTION update_plot ATTRIBUTES(TEXT="Save")
            LET books[ arr_curr() ].b_plot = f_plot
            UPDATE book SET b_plot = f_plot
             WHERE book_id = books[ arr_curr() ].book_id
            MESSAGE SFMT("Plot summary saved (%1)",CURRENT HOUR TO SECOND)

    END INPUT

    ON ACTION close
        ACCEPT DIALOG

    END DIALOG

END MAIN

PRIVATE FUNCTION mbox_yn(question STRING) RETURNS BOOLEAN
     DEFINE r BOOLEAN
     MENU "Books" ATTRIBUTES(STYLE="dialog", COMMENT=question)
         COMMAND "Yes" LET r = TRUE
         COMMAND "No"  LET r = FALSE
     END MENU
     RETURN r
END FUNCTION

FUNCTION fill_book_array(ba t_book_array)
    RETURNS ()

    DEFINE x INTEGER

    DECLARE c1 CURSOR FOR
      SELECT * FROM book ORDER BY b_title

    LET x = 1
    FOREACH c1 INTO ba[x].*
        LET x = x + 1
    END FOREACH
    CALL ba.deleteElement(x)

END FUNCTION

FUNCTION init_authors(e ui.ComboBox)
    RETURNS ()

    DEFINE id LIKE author.auth_id
    DEFINE name LIKE author.a_name

    DECLARE c2 CURSOR FOR
     SELECT auth_id, a_name FROM author ORDER BY a_name

    FOREACH c2 INTO id, name
        CALL e.addItem( id, name )
    END FOREACH

END FUNCTION

FUNCTION create_tables()
   RETURNS ()

   CREATE TABLE author (
       auth_id SERIAL NOT NULL PRIMARY KEY,
       a_name VARCHAR(50)
   );

   CREATE TABLE book (
       book_id SERIAL NOT NULL PRIMARY KEY,
       b_title VARCHAR(100) NOT NULL,
       b_author INTEGER NOT NULL REFERENCES author(auth_id),
       b_isbn VARCHAR(20) NOT NULL UNIQUE,
       b_pub_date DATE,
       b_price DECIMAL(10,2),
       b_plot VARCHAR(500)
   );

   INSERT INTO author VALUES ( 100, '?UNDEFINED?' );

   INSERT INTO author VALUES ( 101, 'Stephen KING' );
   INSERT INTO book VALUES ( 10101, 'The Talisman',  101, '978-0-670-69199-9',  '1984-11-08', 15.60, NULL );
   INSERT INTO book VALUES ( 10102, 'Doctor Sleep',  101, '978-1-4767-2765-3',  '2013-09-24', 12.00, NULL );
   INSERT INTO book VALUES ( 10103, 'The Long Walk', 101, '978-0-451-08754-6',  '1979-07-11', 14.30, NULL );

   INSERT INTO author VALUES ( 103, 'Dan Brown' );
   INSERT INTO book VALUES ( 10301, 'Digital Fortress',  103, '0-312-18087-X',  '1998-01-01', 10.20, NULL );
   INSERT INTO book VALUES ( 10302, 'Angels & Demons',   103, '0-671-02735-2 ', '2000-04-01', 14.55, NULL );

   -- For PostgreSQL
   -- SELECT setval( pg_get_serial_sequence('author','auth_id'), 200 );
   -- SELECT setval( pg_get_serial_sequence('book','book_id'), 20000 );

END FUNCTION

## author^auth_id^262^4^1^
## author^a_name^201^50^2^
## book^book_id^262^4^1^
## book^b_title^457^100^2^
## book^b_author^258^4^3^
## book^b_isbn^457^20^4^
## book^b_pub_date^7^4^5^
## book^b_price^5^2562^6^
## book^b_plot^201^500^7^

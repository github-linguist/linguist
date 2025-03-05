CREATE KEYSPACE booksdb WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };

USE booksdb;

CREATE TABLE books (
   bookid uuid,
   title varchar,
   author varchar,
   genre varchar,
   tags list<varchar>,
   published_date timestamp,
   PRIMARY KEY (bookid)
);

CREATE TABLE book_rating (
   bookid uuid,
   rating_counter counter,
   rating_total counter,
   PRIMARY KEY (bookid)
);

CREATE TABLE genre_index (
   genre varchar,
   bookid uuid,
   timestamp timestamp,
   PRIMARY KEY (genre, bookid)
);

CREATE TABLE comments_by_book (
   bookid uuid,
   comment_ts timestamp,
   comment varchar,
   PRIMARY KEY (bookid, comment_ts)
) WITH CLUSTERING ORDER BY (comment_ts DESC);
ALTER TABLE cycling.cyclist_races
ADD manager UUID;

ALTER TABLE cycling.cyclist_races
ADD completed list<text>;

ALTER TABLE cycling.cyclist_races;

DROP manager;

ALTER TABLE cycling.race_times RENAME race_date TO date;

ALTER TABLE food_cql_conversion.person WITH VERTEX LABEL "person";

ALTER TABLE food_cql_conversion.person_authored_book
WITH EDGE LABEL "authored"
FROM person(person_name, person_id)
TO book(book_name, book_id);

ALTER TABLE food.person RENAME VERTEX LABEL TO "personX";

ALTER TABLE food."person_authored_book" RENAME EDGE LABEL TO "authoredX";

ALTER TABLE food.person WITHOUT VERTEX LABEL "personX";

ALTER TABLE food."person_authored_book" WITHOUT EDGE LABEL "authoredX";

ALTER TABLE cycling.cyclist_base
WITH comment = 'basic cyclist information';

ALTER TABLE cycling.comments WITH caching = {
    'keys' : 'NONE',
    'rows_per_partition' : 10
};

ALTER TABLE cycling.cyclist_base
WITH speculative_retry = '95percentile';

ALTER TABLE cycling.cyclist_base
WITH speculative_retry = '10ms';

ALTER TABLE cycling.comments WITH COMPACTION = {
    'class' : 'SizeTieredCompactionStrategy',
    'enabled' : 'false'
};

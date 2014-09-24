/**
 * sample.pig
 */

REGISTER $SOME_JAR;

A = LOAD 'person' USING PigStorage() AS (name:chararray, age:int); -- Load person
B = FOREACH A generate name;
DUMP B;


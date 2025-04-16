:begin
CREATE CONSTRAINT ON (node:`UNIQUE IMPORT LABEL`) ASSERT (node.`UNIQUE IMPORT ID`) IS UNIQUE;
:commit

:begin
UNWIND [{_id:0, properties:{tagline:"Welcome to the Real World", title:"The Matrix", released:1999}}] AS row
CREATE (n:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row._id}) SET n += row.properties SET n:Movie;

UNWIND [{_id:1, properties:{born:1964, name:"Keanu Reeves"}}, {_id:2, properties:{born:1967, name:"Carrie-Anne Moss"}}, {_id:3, properties:{born:1961, name:"Laurence Fishburne"}}, {_id:4, properties:{born:1960, name:"Hugo Weaving"}}, {_id:5, properties:{born:1967, name:"Lilly Wachowski"}}, {_id:6, properties:{born:1965, name:"Lana Wachowski"}}, {_id:7, properties:{born:1952, name:"Joel Silver"}}] AS row
CREATE (n:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row._id}) SET n += row.properties SET n:Person;
:commit

:begin
UNWIND [{start: {_id:1}, end: {_id:0}, properties:{roles:["Neo"]}}, {start: {_id:2}, end: {_id:0}, properties:{roles:["Trinity"]}}, {start: {_id:3}, end: {_id:0}, properties:{roles:["Morpheus"]}}, {start: {_id:4}, end: {_id:0}, properties:{roles:["Agent Smith"]}}] AS row
MATCH (start:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.start._id})
MATCH (end:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.end._id})
CREATE (start)-[r:ACTED_IN]->(end) SET r += row.properties;

UNWIND [{start: {_id:7}, end: {_id:0}, properties:{}}] AS row
MATCH (start:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.start._id})
MATCH (end:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.end._id})
CREATE (start)-[r:PRODUCED]->(end) SET r += row.properties;

UNWIND [{start: {_id:5}, end: {_id:0}, properties:{}}, {start: {_id:6}, end: {_id:0}, properties:{}}] AS row
MATCH (start:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.start._id})
MATCH (end:`UNIQUE IMPORT LABEL`{`UNIQUE IMPORT ID`: row.end._id})
CREATE (start)-[r:DIRECTED]->(end) SET r += row.properties;
:commit

:begin
MATCH (n:`UNIQUE IMPORT LABEL`)  WITH n LIMIT 20000 REMOVE n:`UNIQUE IMPORT LABEL` REMOVE n.`UNIQUE IMPORT ID`;
:commit

:begin
DROP CONSTRAINT ON (node:`UNIQUE IMPORT LABEL`) ASSERT (node.`UNIQUE IMPORT ID`) IS UNIQUE;
:commit
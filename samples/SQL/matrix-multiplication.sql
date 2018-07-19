CREATE TABLE a (x integer, y integer, e real);
CREATE TABLE b (x integer, y integer, e real);

-- test data
-- A is a 2x2 matrix
INSERT INTO a VALUES(0,0,1); INSERT INTO a VALUES(1,0,2);
INSERT INTO a VALUES(0,1,3); INSERT INTO a VALUES(1,1,4);

-- B is a 2x3 matrix
INSERT INTO b VALUES(0,0,-3); INSERT INTO b VALUES(1,0,-8); INSERT INTO b VALUES(2,0,3);
INSERT INTO b VALUES(0,1,-2); INSERT INTO b VALUES(1,1, 1); INSERT INTO b VALUES(2,1,4);

-- C is 2x2 * 2x3 so will be a 2x3 matrix
SELECT rhs.x, lhs.y, (SELECT sum(a.e*b.e) FROM a, b
                             WHERE a.y = lhs.y
                               AND b.x = rhs.x
                               AND a.x = b.y)
       INTO TABLE c
       FROM a AS lhs, b AS rhs
       WHERE lhs.x = 0 AND rhs.y = 0;

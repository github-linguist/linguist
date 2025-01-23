DROP TABLE TDEPT;
CREATE TABLE TDEPT (DEPTNO CHAR(4));

--#SET TERMINATOR @
BEGIN ATOMIC
  DECLARE COUNT INT DEFAULT 5;

  WHILE COUNT > 0 DO
    INSERT INTO TDEPT VALUES 'F'||
       RTRIM(CHAR(COUNT));
    SET COUNT = COUNT - 1;
  END WHILE;
END@

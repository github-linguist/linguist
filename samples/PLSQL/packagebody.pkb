CREATE OR REPLACE PACKAGE BODY linguistpackage
AS
  /*
  * Package:     linguist pacakage body
  * Purpose:     a sample PLSQL file for linguist to work with
  *
  * Date:        03/03/2014
  * Author:      david pyke le brun
  * Comments:    initial version
  */

PROCEDURE proc_1
IS
BEGIN
NULL;
END;

-- functions with 1 arg
FUNCTION function1( param1 VARCHAR2 ) RETURN VARCHAR2
IS
CURSOR c IS
select * from dual;
v c%ROWTYPE;
BEGIN
 open c;
 fetch c into v;
 close c;

 return v;
end;

FUNCTION function2( param1 NUMBER ) RETURN DATE
IS
BEGIN
 return SYSDATE;
end;

--a few more to use all basic SQL types
FUNCTION function3( param1 TIMESTAMP ) RETURN CHAR
IS
BEGIN
IF 1 = 2 THEN
return 'Y';
ELSE
return 'N';
END IF;
return NULL;
END;


FUNCTION function4( param1 CLOB ) RETURN BLOB
IS
BEGIN
	return null;
END;

END linguistpackage;
/

CREATE OR REPLACE PACKAGE linguistpackage
AUTHID DEFINER
AS
  /*
  * Package:     linguist pacakage
  * Purpose:     a sample PLSQL file for linguist to work with
  *
  * Date:        03/03/2014
  * Author:      david pyke le brun
  * Comments:    initial version
  */

k_constant CONSTANT NUMBER(10,2) := 3.14;

--basic procedure
PROCEDURE proc_1;

-- functions with 1 arg
FUNCTION function1( param1 VARCHAR2 ) RETURN VARCHAR2;
FUNCTION function2( param1 NUMBER ) RETURN DATE;

--a few more to use all basic SQL types
FUNCTION function3( param1 TIMESTAMP ) RETURN CHAR;
FUNCTION function4( param1 CLOB ) RETURN BLOB;
  
END linguistpackage;
/


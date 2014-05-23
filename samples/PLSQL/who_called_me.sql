CREATE OR REPLACE PROCEDURE who_called_me
( owner      OUT VARCHAR2,
  name       OUT VARCHAR2,
  lineno     OUT NUMBER,
  caller_t   OUT VARCHAR2 ,
  depth          NUMBER DEFAULT 1
)
AUTHID DEFINER
AS
--depth based version of who_called_me from asktom
   call_stack  VARCHAR2(4096) default dbms_utility.format_call_stack;
   n           NUMBER;
   found_stack BOOLEAN DEFAULT FALSE;
   line        VARCHAR2(255);
   cnt         NUMBER := 0;
BEGIN
   LOOP
       n := instr( call_stack, chr(10) );
       exit when ( n is NULL or n = 0 );
--
       line := substr( call_stack, 1, n-1 );
       call_stack := substr( call_stack, n+1 );
--
       if ( NOT found_stack ) then
           if ( line like '%handle%number%name%' ) then
               found_stack := TRUE;
           end if;
       else
           cnt := cnt + 1;
           -- cnt = 1 is ME
           -- cnt = 2 is MY Caller
           -- cnt = 3 is Their Caller
           if ( cnt = (2+depth) ) then
               lineno := to_number(substr( line, 13, 8 ));
               line   := substr( line, 23 ); --set to rest of line .. change from 21 to 23
               if ( line like 'pr%' ) then
                   n := length( 'procedure ' );
               elsif ( line like 'fun%' ) then
                   n := length( 'function ' );
               elsif ( line like 'package body%' ) then
                   n := length( 'package body ' );
               elsif ( line like 'pack%' ) then
                   n := length( 'package ' );
               elsif ( line like 'anonymous%' ) then
                   n := length( 'anonymous block ' );
               else
                   n := null;
               end if;
               if ( n is not null ) then
                  caller_t := ltrim(rtrim(upper(substr( line, 1, n-1 ))));
               else
                  caller_t := 'TRIGGER';
               end if;

               line := substr( line, nvl(n,1) );
               n := instr( line, '.' );
               owner := ltrim(rtrim(substr( line, 1, n-1 )));
               name  := LTRIM(RTRIM(SUBSTR( LINE, N+1 )));
               exit;
           END IF;
       END IF;
   END LOOP;
END;
/


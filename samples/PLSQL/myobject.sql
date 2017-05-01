create or replace type myobject
AUTHID DEFINER
AS OBJECT
(
        m_name varchar2(200),
        member function toString RETURN VARCHAR2,
        map member function Compare return varchar2

)
not instantiable not final;
/

prompt create type myarray
create or replace type myarray as table of myobject;
/

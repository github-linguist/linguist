create or replace procedure print_bool(
    p_bool in BOOLEAN,
    p_true_value in varchar2 default 'TRUE',
    p_false_value in varchar2 := 'FALSE'
)
as
begin

    dbms_output.put_line(case when p_bool then p_true_value else p_false_value end);

end print_bool;
/

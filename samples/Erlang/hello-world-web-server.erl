-module( hello_world_web_server ).

-export( [do/1, httpd_start/2, httpd_stop/1, task/0] ).

do( _Data ) ->
  {proceed, [{response,{200,"Goodbye, World!"}}]}.

httpd_start( Port, Module ) ->
  Arguments = [{bind_address, "localhost"}, {port, Port}, {ipfamily, inet},
    {modules, [Module]},
    {server_name,erlang:atom_to_list(Module)}, {server_root,"."}, {document_root,"."}],
  {ok, Pid} = inets:start( httpd, Arguments, stand_alone ),
  Pid.

httpd_stop( Pid ) ->
  inets:stop( stand_alone, Pid ).

task() ->
  Pid = httpd_start( 8080, ?MODULE ),
  timer:sleep( 30000 ),
  httpd_stop( Pid ).

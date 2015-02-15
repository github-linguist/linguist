declare
  functor ServerCode
  export
     port:Prt
  define
     Stream
     Prt = {NewPort ?Stream}
     thread
	for Request#Reply in Stream do
	   case Request
	   of echo(Data)        then Reply = Data
	   [] compute(Function) then Reply = {Function}
	   end
	end
     end
  end

  %% create the server on some machine
  %% (just change "localhost" to some machine
  %% that you can use with a passwordless rsh login
  %% and that has the same Mozart version installed)
  RM = {New Remote.manager init(host:localhost)}

  %% execute the code encapsulated in the ServerCode functor
  Server = {RM apply(ServerCode $)}

  %% Shortcut: send a message to Server and receive a reply
  fun {Send X}
     {Port.sendRecv Server.port X}
  end
in
  %% echo
  {System.showInfo "Echo reply: "#{Send echo(hello)}}

  %% compute
  {System.showInfo "Result of computation: "#
   {Send compute(fun {$} 8 div 4 end)}}

  %% shut down server
  {RM close}

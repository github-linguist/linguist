declare
  ParentVar1 = "parent data"
  ParentVar2

  functor RemoteCode
  export
     result:Result
  import QTk at 'x-oz://system/wp/QTk.ozf'
  define
     Result
     %% Show a simple window. When it is closed by the user, set Result.
     Window =
     {QTk.build
      td(action:proc {$} Result = 42 end %% on close
         label(text:"In child process: "#ParentVar1))} %% read parent process variable
     {Window show}
     !ParentVar2 = childData %% write to parent process variable
     {Wait Result}
  end

  %% create a new process on the same machine
  RM = {New Remote.manager init(host:localhost)}
  %% execute the code encapsulated in the given functor
  RemoteModule = {RM apply(RemoteCode $)}
in
  %% retrieve data from child process
  {Show RemoteModule.result} %% prints 42
  %% exit child process
  {RM close}
  {Show ParentVar2} %% print "childData"

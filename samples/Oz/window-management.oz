declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}

  %% The messages that can be sent to the windows.
  WindowActions =
  [hide show close
   iconify deiconify
   maximize restore
   set(minsize:minsize(width:400 height:400))
   set(minsize:minsize(width:200 height:200))
   set(geometry:geometry(x:0 y:0))
   set(geometry:geometry(x:500 y:500))
  ]

  %% Two windows, still uninitialized.
  Windows = windows(window1:_
                    window2:_)

  fun {CreateWindow}
     Message = {NewCell WindowActions.1}
     ReceiverName = {NewCell {Arity Windows}.1}
     fun {ButtonText}
        "Send"#"  "#{ValueToString @Message}#"  to  "#@ReceiverName
     end
     Button
     Desc =
     td(title:"Window Management"
        lr(listbox(init:{Arity Windows}
                   glue:nswe
                   tdscrollbar:true
                   actionh:proc {$ W}
                              ReceiverName := {GetSelected W}
                              {Button set(text:{ButtonText})}
                           end
                  )
           listbox(init:{Map WindowActions ValueToString}
                   glue:nswe
                   tdscrollbar:true
                   actionh:proc {$ A}
                              Message := {GetSelected A}
                              {Button set(text:{ButtonText})}
                           end
                  )
           glue:nswe
          )
        button(text:{ButtonText}
               glue:we
               handle:Button
               action:proc {$}
                         {Windows.@ReceiverName @Message}
                      end
              )
       )
     Window = {Extend {QTk.build Desc}}
  in
     {Window show}
     Window
  end

  %% Adds two methods to a toplevel instance.
  %% For maximize and restore we have to interact directly with Tk
  %% because that functionality is not part of the QTk library.
  fun {Extend Toplevel}
     proc {$ A}
        case A of maximize then
           {Tk.send wm(state Toplevel zoomed)}
        [] restore then
           {Tk.send wm(state Toplevel normal)}
        else
           {Toplevel A}
        end
     end
  end

  %% Returns the current entry of a listbox
  %% as an Oz value.
  fun {GetSelected LB}
     Entries = {LB get($)}
     Index = {LB get(firstselection:$)}
  in
     {Compiler.virtualStringToValue {Nth Entries Index}}
  end

  fun {ValueToString V}
     {Value.toVirtualString V 100 100}
  end
in
  {Record.forAll Windows CreateWindow}

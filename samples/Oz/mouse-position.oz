declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  WindowClosed = {NewCell false}
  Label
  Window = {QTk.build
	    td(action:proc {$} WindowClosed := true {Window close} end
	       label(text:"" handle:Label))}
in
  {Window show}

  for while:{Not @WindowClosed} do
     TopmostWindow = {List.last {String.tokens {Tk.return wm(stackorder '.')} & }}
     Winfo = {Record.mapInd winfo(rootx:_ rooty:_ pointerx:_ pointery:_)
	      fun {$ I _}
		 {Tk.returnInt winfo(I TopmostWindow)}
	      end}
  in
     {Label set(text:"x: "#(Winfo.pointerx - Winfo.rootx)
		#", y: "#(Winfo.pointery - Winfo.rooty))}
     {Delay 250}
  end

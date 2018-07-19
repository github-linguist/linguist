declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Label
  Window = {QTk.build td(label(text:"Hello" handle:Label))}
in
  {Window bind(event:"<Control-x><Control-s>"
	       action:proc {$}
			 {Label set(text:"This is not Emacs.")}
		      end
	      )}
  {Window bind(event:"<Control-x><Control-c>"
	       action:toplevel#close
	      )}
  {Window show}

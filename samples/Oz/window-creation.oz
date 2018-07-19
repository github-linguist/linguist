functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   proc {OnClose}
      {Application.exit 0}
   end

   %% Descripe the GUI in a declarative style.
   GUIDescription = td(label(text:"Hello World!")
		       action:OnClose %% Exit app when window closes.
		      )

   %% Create a window object from the description and show it.
   Window = {QTk.build GUIDescription}
   {Window show}
end

with Gtk.Button;  use Gtk.Button;
with Gtk.GEntry;  use Gtk.GEntry;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Table;   use Gtk.Table;

with Gtk.Handlers;
with Gtk.Main;

procedure Graphic_Input is
   Window  : Gtk_Window;
   Grid    : Gtk_Table;
   Label   : Gtk_Label;
   Message : Gtk_Label;
   Edit    : Gtk_GEntry;
   Button  : Gtk_Button;

   package Handlers is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   package Return_Handlers is
      new Gtk.Handlers.Return_Callback (Gtk_Widget_Record, Boolean);

   function Delete_Event (Widget : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      return False;
   end Delete_Event;

   procedure Destroy (Widget : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Destroy;

   procedure Clicked (Widget : access Gtk_Widget_Record'Class) is
   begin
      if Get_Text (Label) = "Enter integer:" then
         Set_Text (Message, "Entered:" & Integer'Image (Integer'Value (Get_Text (Edit))));
         Set_Sensitive (Button, False);
      else
         Set_Text (Message, "Entered:" & Get_Text (Edit));
         Set_Text (Label, "Enter integer:");
      end if;
   exception
      when Constraint_Error =>
         Set_Text (Message, "Error integer input");
   end Clicked;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Grid, 2, 3, False);
   Add (Window, Grid);
   Gtk_New (Label, "Enter string:");
   Attach (Grid, Label, 0, 1, 0, 1);
   Gtk_New (Edit);
   Attach (Grid, Edit, 1, 2, 0, 1);
   Gtk_New (Button, "OK");
   Attach (Grid, Button, 2, 3, 0, 1);
   Gtk_New (Message);
   Attach (Grid, Message, 0, 3, 1, 2);
   Return_Handlers.Connect
   (  Window,
      "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access)
   );
   Handlers.Connect
   (  Window,
      "destroy",
      Handlers.To_Marshaller (Destroy'Access)
   );
   Handlers.Connect
   (  Button,
      "clicked",
      Handlers.To_Marshaller (Clicked'Access)
   );
   Show_All (Grid);
   Show (Window);

   Gtk.Main.Main;
end Graphic_Input;

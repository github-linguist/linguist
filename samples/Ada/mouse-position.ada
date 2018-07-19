with GLib;        use GLib;
with Gtk.Button;  use Gtk.Button;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Table;   use Gtk.Table;

with Gtk.Handlers;
with Gtk.Main;

procedure Tell_Mouse is
   Window : Gtk_Window;
   Grid   : Gtk_Table;
   Button : Gtk_Button;
   Label  : Gtk_Label;

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
      X, Y : GInt;
   begin
      Get_Pointer (Window, X, Y);
      Set_Text (Label, "At" & GInt'Image (X) & GInt'Image (Y));
   end Clicked;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Grid, 1, 2, False);
   Add (Window, Grid);
   Gtk_New (Label);
   Attach (Grid, Label, 0, 1, 0, 1);
   Gtk_New (Button, "Click me");
   Attach (Grid, Button, 0, 1, 1, 2);
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
end Tell_Mouse;

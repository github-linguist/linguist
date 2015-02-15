with Gdk.Event;   use Gdk.Event;
with Gtk.Label;   use Gtk.Label;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;

with Gtk.Handlers;
with Gtk.Main;

procedure Windowed_Goodbye_World is
   Window : Gtk_Window;
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

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk_New (Label, "Goodbye, World!");
   Add (Window, Label);
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
   Show_All (Label);
   Show (Window);

   Gtk.Main.Main;
end Windowed_Goodbye_World;

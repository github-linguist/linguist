with Gtk.Main;
with Glib;
with Gtk.Window;  use Gtk.Window;
with Gtk.Enums;   use Gtk.Enums;
with Ada.Text_IO; use Ada.Text_IO;

procedure Max_Size is

   Win          : Gtk_Window;
   Win_W, Win_H : Glib.Gint;
   package Int_Io is new Integer_IO (Glib.Gint);
   Hid : Gtk.Main.Quit_Handler_Id;

begin
   Gtk.Main.Init;
   Gtk_New (Win);
   Initialize (Win, Window_Toplevel);
   Maximize (Win);
   Show (Win);
   Get_Size (Win, Win_W, Win_H);
   Put ("Maximum dimensions of window : W ");
   Int_Io.Put (Win_W, Width => 4);
   Put (" x H ");
   Int_Io.Put (Win_H, Width => 4);
   New_Line;
   Hid := Gtk.Main.Quit_Add_Destroy (0, Win);
end Max_Size;

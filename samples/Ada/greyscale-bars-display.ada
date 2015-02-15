with Gtk.Window;   use Gtk.Window;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;
with Gdk;
with Gdk.Event;
with Glib;         use Glib;
with Cairo;        use Cairo;
with Gdk.Cairo;
pragma Elaborate_All (Gtk.Handlers);

procedure Greyscale is

   Win    : Gtk_Window;
   Width  : constant := 640;
   Height : constant := 512;

   package Handlers is new Gtk.Handlers.Callback (Gtk_Window_Record);
   package Event_Cb is new Gtk.Handlers.Return_Callback (
      Widget_Type => Gtk_Window_Record,
      Return_Type => Boolean);

   procedure Quit (Win : access Gtk_Window_Record'Class) is
      pragma Warnings (Off, Win);
   begin
      Gtk.Main.Main_Quit;
   end Quit;

   function Expose
     (Drawing : access Gtk_Window_Record'Class;
      Event   : Gdk.Event.Gdk_Event)
      return    Boolean
   is
      subtype Dub is Glib.Gdouble;
      Cr       : Cairo_Context;
      Revert   : Boolean;
      Grey     : Dub;
      DH       : constant Dub := Dub (Height) / 4.0;
      X, Y, DW : Dub;
      N        : Natural;

   begin
      Cr := Gdk.Cairo.Create (Get_Window (Drawing));
      for Row in 1 .. 4 loop

         N      := 2 ** (Row + 2);
         Revert := (Row mod 2) = 0;
         DW     := Dub (Width) / Dub (N);
         X      := 0.0;
         Y      := DH * Dub (Row - 1);
         for B in 0 .. (N - 1) loop
            Grey := Dub (B) / Dub (N - 1);
            if Revert then
               Grey := 1.0 - Grey;
            end if;
            Cairo.Set_Source_Rgb (Cr, Grey, Grey, Grey);
            Cairo.Rectangle (Cr, X, Y, DW, DH);
            Cairo.Fill (Cr);
            X := X + DW;
         end loop;
      end loop;
      Cairo.Destroy (Cr);
      return False;
   end Expose;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Win);
   Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
   Set_Title (Win, "Greyscale with GTKAda");
   Set_Default_Size (Win, Width, Height);
   Set_App_Paintable (Win, True);
   --  Attach handlers
   Handlers.Connect (Win, "destroy", Handlers.To_Marshaller (Quit'Access));
   Event_Cb.Connect
     (Win,
      "expose_event",
      Event_Cb.To_Marshaller (Expose'Access));

   Show_All (Win);

   Gtk.Main.Main;
end Greyscale;

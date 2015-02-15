with Ada.Strings.Fixed;
with Gtk.Main;
with Gtk.Handlers;
with Gtk.Button;
with Gtk.Window;
with Gtk.GEntry;
with Gtk.Editable;
with Gtk.Box;
with Gtk.Widget;
with Glib.Values;

procedure Disabling is
   type My_Natural is range 0 .. 10;

   The_Value : My_Natural := 0;

   Main_Window      : Gtk.Window.Gtk_Window;
   Content          : Gtk.Box.Gtk_Vbox;
   Increment_Button : Gtk.Button.Gtk_Button;
   Decrement_Button : Gtk.Button.Gtk_Button;
   Entry_Field      : Gtk.GEntry.Gtk_Entry;

   package Entry_Callbacks is new Gtk.Handlers.Callback
     (Gtk.GEntry.Gtk_Entry_Record);

   package Button_Callbacks is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   package Window_Callbacks is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record, Boolean);

   --  update displayed text
   procedure Update_Entry is
   begin
      Gtk.GEntry.Set_Text
        (The_Entry => Entry_Field,
         Text      =>
           Ada.Strings.Fixed.Trim
             (Source => My_Natural'Image (The_Value),
              Side   => Ada.Strings.Both));
   end Update_Entry;

   procedure Check_Value is
   begin
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Decrement_Button),
         The_Value > 0);
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Increment_Button),
         The_Value < 10);
      Gtk.Widget.Set_Sensitive
        (Gtk.Widget.Gtk_Widget (Entry_Field),
         The_Value = 0);
   end Check_Value;

   procedure On_Changed_Text
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params, Object);
   begin
      The_Value := My_Natural'Value (Gtk.GEntry.Get_Text (Entry_Field));
      Check_Value;
      Update_Entry;
   exception
      when Constraint_Error =>
         The_Value := 0;
   end On_Changed_Text;

   --  make sure that only numbers are entered
   procedure On_Insert_Text
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Params : Glib.Values.GValues)
   is
      Length : constant Glib.Gint :=
        Glib.Values.Get_Int (Glib.Values.Nth (Params, 2));
      Text   : constant String    :=
        Glib.Values.Get_String (Glib.Values.Nth (Params, 1), Length);
   begin
      declare
         Number : My_Natural;
         pragma Unreferenced (Number);
      begin
         Number := My_Natural'Value (Text);
      exception
         when Constraint_Error =>
            --  refuse values that are not parsable
            Gtk.Handlers.Emit_Stop_By_Name
              (Object => Object,
               Name   => Gtk.Editable.Signal_Insert_Text);
      end;
   end On_Insert_Text;

   --  Callback for click event
   procedure On_Increment_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      The_Value := The_Value + 1;
      Check_Value;
      Update_Entry;
   end On_Increment_Click;

   --  Callback for click event
   procedure On_Decrement_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      The_Value := The_Value - 1;
      Check_Value;
      Update_Entry;
   end On_Decrement_Click;

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

begin

   Gtk.Main.Init;

   Gtk.GEntry.Gtk_New (Widget => Entry_Field);
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Insert_Text,
      Cb     => On_Insert_Text'Access);
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Changed,
      Cb     => On_Changed_Text'Access);

   Gtk.Button.Gtk_New (Button => Increment_Button, Label => "Increment");
   Gtk.Button.Gtk_New (Button => Decrement_Button, Label => "Decrement");

   Button_Callbacks.Connect
     (Widget => Increment_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Increment_Click'Access));
   Button_Callbacks.Connect
     (Widget => Decrement_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Decrement_Click'Access));

   Gtk.Box.Gtk_New_Vbox (Box => Content);
   Gtk.Box.Add (Container => Content, Widget => Entry_Field);
   Gtk.Box.Add (Container => Content, Widget => Increment_Button);
   Gtk.Box.Add (Container => Content, Widget => Decrement_Button);

   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Window.Add (Container => Main_Window, Widget => Content);

   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => Gtk.Widget.Signal_Delete_Event,
      Cb     => On_Main_Window_Delete'Access);
   Gtk.Window.Show_All (Widget => Main_Window);
   Update_Entry;

   Gtk.Main.Main;
end Disabling;

with Ada.Numerics.Discrete_Random;
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
with Gtkada.Dialogs;

procedure Interaction is

   The_Value : Natural := 0;

   package Natural_Random is new Ada.Numerics.Discrete_Random (Natural);
   RNG : Natural_Random.Generator;

   Main_Window      : Gtk.Window.Gtk_Window;
   Content          : Gtk.Box.Gtk_Vbox;
   Increment_Button : Gtk.Button.Gtk_Button;
   Random_Button    : Gtk.Button.Gtk_Button;
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
             (Source => Natural'Image (The_Value),
              Side   => Ada.Strings.Both));
   end Update_Entry;

   --  read from text entry
   procedure Update_Value is
   begin
      The_Value := Natural'Value (Gtk.GEntry.Get_Text (Entry_Field));
   exception
      when Constraint_Error =>
         The_Value := 0;
   end Update_Value;

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
         Number : Natural;
      begin
         Number := Natural'Value (Text);
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
   begin
      Update_Value;
      The_Value := The_Value + 1;
      Update_Entry;
   end On_Increment_Click;

   --  Callback for click event
   procedure On_Random_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use type Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Gtkada.Dialogs.Message_Dialog
        (Msg            => "Really reset to random value?",
         Dialog_Type    => Gtkada.Dialogs.Confirmation,
         Buttons        => Gtkada.Dialogs.Button_Yes or
           Gtkada.Dialogs.Button_No,
         Default_Button => Gtkada.Dialogs.Button_Yes) =
        Gtkada.Dialogs.Button_Yes
      then
         The_Value := Natural_Random.Random (RNG);
         Update_Entry;
      end if;
   end On_Random_Click;

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

begin
   --  initialize random number generator
   Natural_Random.Reset (RNG);

   Gtk.Main.Init;

   Gtk.GEntry.Gtk_New (Widget => Entry_Field);
   Update_Entry;
   Entry_Callbacks.Connect
     (Widget => Entry_Field,
      Name   => Gtk.Editable.Signal_Insert_Text,
      Cb     => On_Insert_Text'Access);

   Gtk.Button.Gtk_New (Button => Increment_Button, Label => "Increment");
   Gtk.Button.Gtk_New (Button => Random_Button, Label => "Random");

   Button_Callbacks.Connect
     (Widget => Increment_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Increment_Click'Access));
   Button_Callbacks.Connect
     (Widget => Random_Button,
      Name   => Gtk.Button.Signal_Clicked,
      Marsh  => Button_Callbacks.To_Marshaller (On_Random_Click'Access));

   Gtk.Box.Gtk_New_Vbox (Box => Content);
   Gtk.Box.Add (Container => Content, Widget => Entry_Field);
   Gtk.Box.Add (Container => Content, Widget => Increment_Button);
   Gtk.Box.Add (Container => Content, Widget => Random_Button);

   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Window.Add (Container => Main_Window, Widget => Content);

   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => Gtk.Widget.Signal_Delete_Event,
      Cb     => On_Main_Window_Delete'Access);
   Gtk.Window.Show_All (Widget => Main_Window);

   Gtk.Main.Main;
end Interaction;

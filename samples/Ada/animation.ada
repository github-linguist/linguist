with Gtk.Main;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Button;
with Gtk.Window;
with Glib.Main;

procedure Animation is
   Scroll_Forwards : Boolean := True;

   package Button_Callbacks is new Gtk.Handlers.Callback
     (Gtk.Button.Gtk_Button_Record);

   package Label_Timeout is new Glib.Main.Generic_Sources
     (Gtk.Label.Gtk_Label);

   package Window_Callbacks is new Gtk.Handlers.Return_Callback
     (Gtk.Window.Gtk_Window_Record, Boolean);

   --  Callback for click event
   procedure On_Button_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class);

   --  Callback for delete event
   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean;

   function Scroll_Text (Data : Gtk.Label.Gtk_Label) return Boolean;

   procedure On_Button_Click
     (Object : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Scroll_Forwards := not Scroll_Forwards;
   end On_Button_Click;

   function On_Main_Window_Delete
     (Object : access Gtk.Window.Gtk_Window_Record'Class)
      return   Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
      return True;
   end On_Main_Window_Delete;

   function Scroll_Text (Data : Gtk.Label.Gtk_Label) return Boolean is
      Text : constant String := Gtk.Label.Get_Text (Data);
   begin
      if Scroll_Forwards then
         Gtk.Label.Set_Text
           (Label => Data,
            Str   => Text (Text'First + 1 .. Text'Last) & Text (Text'First));
      else
         Gtk.Label.Set_Text
           (Label => Data,
            Str   => Text (Text'Last) & Text (Text'First .. Text'Last - 1));
      end if;
      return True;
   end Scroll_Text;

   Main_Window     : Gtk.Window.Gtk_Window;
   Text_Button     : Gtk.Button.Gtk_Button;
   Scrolling_Text  : Gtk.Label.Gtk_Label;
   Timeout_ID      : Glib.Main.G_Source_Id;
   pragma Unreferenced (Timeout_ID);

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window => Main_Window);
   Gtk.Label.Gtk_New (Label => Scrolling_Text, Str => "Hello World! ");
   Gtk.Button.Gtk_New (Button => Text_Button);
   Gtk.Button.Add (Container => Text_Button, Widget => Scrolling_Text);
   Button_Callbacks.Connect
     (Widget => Text_Button,
      Name   => "clicked",
      Marsh  => Button_Callbacks.To_Marshaller (On_Button_Click'Access));
   Timeout_ID :=
     Label_Timeout.Timeout_Add
       (Interval => 125,
        Func     => Scroll_Text'Access,
        Data     => Scrolling_Text);
   Gtk.Window.Add (Container => Main_Window, Widget => Text_Button);
   Window_Callbacks.Connect
     (Widget => Main_Window,
      Name   => "delete_event",
      Marsh  => Window_Callbacks.To_Marshaller (On_Main_Window_Delete'Access));
   Gtk.Window.Show_All (Widget => Main_Window);
   Gtk.Main.Main;
end Animation;

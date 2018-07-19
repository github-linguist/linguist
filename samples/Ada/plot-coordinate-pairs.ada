with Gtk.Main;
with Gtk.Window;  use Gtk.Window;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Handlers;  use Gtk.Handlers;
with Glib;  use Glib;
with Gtk.Extra.Plot;  use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Gtk.Extra.Plot_Canvas;  use Gtk.Extra.Plot_Canvas;
with Gtk.Extra.Plot_Canvas.Plot;  use Gtk.Extra.Plot_Canvas.Plot;

procedure PlotCoords is
   package Handler is new Callback (Gtk_Widget_Record);

   Window : Gtk_Window;
   Plot : Gtk_Plot;
   PCP : Gtk_Plot_Canvas_Plot;
   Canvas : Gtk_Plot_Canvas;
   PlotData : Gtk_Plot_Data;
   x, y, dx, dy : Gdouble_Array_Access;

   procedure ExitMain (Object : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Object);  Gtk.Main.Main_Quit;
   end ExitMain;

begin
   x := new Gdouble_Array'(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
   y := new Gdouble_Array'(2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0);
   Gtk.Main.Init;
   Gtk_New (Window);
   Set_Title (Window, "Plot coordinate pairs with GtkAda");
   Gtk_New (PlotData);
   Set_Points (PlotData, x, y, dx, dy);
   Gtk_New (Plot);
   Add_Data (Plot, PlotData);
   Autoscale (Plot);  Show (PlotData);
   Hide_Legends (Plot);
   Gtk_New (PCP, Plot);  Show (Plot);
   Gtk_New (Canvas, 500, 500);  Show (Canvas);
   Put_Child (Canvas, PCP, 0.15, 0.15, 0.85, 0.85);
   Add (Window, Canvas);
   Show_All (Window);
   Handler.Connect (Window, "destroy",
      Handler.To_Marshaller (ExitMain'Access));
   Gtk.Main.Main;
end PlotCoords;

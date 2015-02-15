program HelloWorldGraphical;

uses
  glib2, gdk2, gtk2;

var
  window: PGtkWidget;

begin
  gtk_init(@argc, @argv);

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (GTK_WINDOW (window), 'Goodbye, World');
  g_signal_connect (G_OBJECT (window),
                    'delete-event',
        G_CALLBACK (@gtk_main_quit),
        NULL);
  gtk_widget_show_all (window);

  gtk_main();
end.

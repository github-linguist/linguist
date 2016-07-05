%{^
extern
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv);
%}

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"
staload "contrib/GTK/SATS/gtk.sats"

extern fun main1 (): void = "main1"

implement main1 () = () where {
  val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  val (pf | title) = gstring_of_string ("Goodbye, World!")
  val () = gtk_window_set_title (window, title)
  prval () = pf (title)
  val _sid = g_signal_connect
    (window, (gsignal)"delete-event", (G_CALLBACK)gtk_main_quit, (gpointer)null)
  val () = gtk_widget_show (window)
  val () = gtk_main ()
  val () = gtk_widget_destroy0 (window)
}

implement main_dummy () = ()

%{$
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv)
{
  gtk_init ((int*)&argc, (char***)&argv);
  main1 ();
  return;
}
%}

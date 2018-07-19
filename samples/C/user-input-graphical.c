#include <gtk/gtk.h>

void ok_hit(GtkButton *o, GtkWidget **w)
{
  GtkMessageDialog *msg;

  gdouble v = gtk_spin_button_get_value((GtkSpinButton *)w[1]);
  const gchar *c = gtk_entry_get_text((GtkEntry *)w[0]);

  msg = (GtkMessageDialog *)
    gtk_message_dialog_new(NULL,
			   GTK_DIALOG_MODAL,
			   (v==75000) ? GTK_MESSAGE_INFO : GTK_MESSAGE_ERROR,
			   GTK_BUTTONS_OK,
			   "You wrote '%s' and selected the number %d%s",
			   c, (gint)v,
			   (v==75000) ? "" : " which is wrong (75000 expected)!");
  gtk_widget_show_all(GTK_WIDGET(msg));
  (void)gtk_dialog_run(GTK_DIALOG(msg));
  gtk_widget_destroy(GTK_WIDGET(msg));
  if ( v==75000 ) gtk_main_quit();
}

int main(int argc, char **argv)
{
  GtkWindow *win;
  GtkEntry *entry;
  GtkSpinButton *spin;
  GtkButton *okbutton;
  GtkLabel *entry_l, *spin_l;
  GtkHBox *hbox[2];
  GtkVBox *vbox;
  GtkWidget *widgs[2];

  gtk_init(&argc, &argv);

  win = (GtkWindow *)gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(win, "Insert values");

  entry_l = (GtkLabel *)gtk_label_new("Insert a string");
  spin_l =  (GtkLabel *)gtk_label_new("Insert 75000");

  entry = (GtkEntry *)gtk_entry_new();
  spin = (GtkSpinButton *)gtk_spin_button_new_with_range(0, 80000, 1);

  widgs[0] = GTK_WIDGET(entry);
  widgs[1] = GTK_WIDGET(spin);

  okbutton = (GtkButton *)gtk_button_new_with_label("Ok");

  hbox[0] = (GtkHBox *)gtk_hbox_new(FALSE, 1);
  hbox[1] = (GtkHBox *)gtk_hbox_new(FALSE, 1);

  vbox = (GtkVBox *)gtk_vbox_new(TRUE, 1);

  gtk_container_add(GTK_CONTAINER(hbox[0]), GTK_WIDGET(entry_l));
  gtk_container_add(GTK_CONTAINER(hbox[0]), GTK_WIDGET(entry));
  gtk_container_add(GTK_CONTAINER(hbox[1]), GTK_WIDGET(spin_l));
  gtk_container_add(GTK_CONTAINER(hbox[1]), GTK_WIDGET(spin));

  gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(hbox[0]));
  gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(hbox[1]));
  gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(okbutton));

  gtk_container_add(GTK_CONTAINER(win), GTK_WIDGET(vbox));

  g_signal_connect(G_OBJECT(win), "delete-event", (GCallback)gtk_main_quit, NULL);
  g_signal_connect(G_OBJECT(okbutton), "clicked", (GCallback)ok_hit, widgs);

  gtk_widget_show_all(GTK_WIDGET(win));
  gtk_main();

  return 0;
}

#include <stdio.h>
#include <gtk/gtk.h>

const gchar *clickme = "Click Me";
guint counter = 0;

#define MAXLEN 64
void clickedme(GtkButton *o, gpointer d)
{
    GtkLabel *l = GTK_LABEL(d);
    char nt[MAXLEN];

    counter++;
    snprintf(nt, MAXLEN, "You clicked me %d times", counter);
    gtk_label_set_text(l, nt);
}

int main(int argc, char **argv)
{
    GtkWindow *win;
    GtkButton *button;
    GtkLabel *label;
    GtkVBox *vbox;

    gtk_init(&argc, &argv);
    win = (GtkWindow*)gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(win, clickme);
    button = (GtkButton*)gtk_button_new_with_label(clickme);
    label = (GtkLabel*)gtk_label_new("There have been no clicks yet");
    gtk_label_set_single_line_mode(label, TRUE);
    vbox = (GtkVBox*)gtk_vbox_new(TRUE, 1);
    gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(label));
    gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(button));
    gtk_container_add(GTK_CONTAINER(win), GTK_WIDGET(vbox));
    g_signal_connect(G_OBJECT(win), "delete-event", (GCallback)gtk_main_quit, NULL);
    g_signal_connect(G_OBJECT(button), "clicked", (GCallback)clickedme, label);
    gtk_widget_show_all(GTK_WIDGET(win));
    gtk_main();
    return 0;
}

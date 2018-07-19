#include <gtk/gtk.h>
/* do some greyscale plotting */
void gsplot (cairo_t *cr,int x,int y,double s) {
    cairo_set_source_rgb (cr,s,s,s);
    cairo_move_to (cr,x+0.5,y);
    cairo_rel_line_to (cr,0,1);
    cairo_stroke (cr);
}
/* make a shaded widget */
gboolean expose_event (GtkWidget *widget,GdkEventExpose *event,gpointer data) {
    int r,c,x=0;
    cairo_t *cr;
    cr = gdk_cairo_create (widget->window);
    cairo_scale (cr,5,50);
    cairo_set_line_width (cr,1);
    for (r=0;r<4;r++) {
        c = (r&1)*64-(r%2);
        do gsplot (cr,x++%64,r,c/(1<<(3-r))/(8*(1<<r)-1.0));
        while ((c+=2*!(r%2)-1)!=(!(r%2))*64-(r%2));
    } cairo_destroy (cr);
    return FALSE;
}
/* main */
int main (int argc, char *argv[]) {
    GtkWidget *window;
    gtk_init (&argc, &argv);
    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_signal_connect (window, "expose-event",G_CALLBACK (expose_event), NULL);
    g_signal_connect (window, "delete-event", G_CALLBACK(gtk_main_quit), NULL);
    gtk_window_set_default_size (GTK_WINDOW(window), 320, 200);
    gtk_widget_set_app_paintable (window, TRUE);
    gtk_widget_show_all (window);
    gtk_main ();
    return 0;
}

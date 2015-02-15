/* Program for gtk3 */
/* discovery: essential to use consistent documentation */
/* compilation on linux: */
/* $ a=./hexagon && make -k "CFLAGS=$( pkg-config --cflags gtk+-3.0 )" "LOADLIBES=$( pkg-config --libs gtk+-3.0 )" $a && $a --gtk-debug=all */
/* search for  to do */
/* The keyboard and mouse callbacks increment the "selected" status, */
/* of the matching hexagon, */
/* then invalidate the drawing window which triggers a draw event. */
/* The draw callback redraws the screen and tests for completion, */
/* upon which the program spits back the characters selected and exits */

#include<math.h>
#include<string.h>
#include<stdlib.h>
#include<gtk/gtk.h>

static GdkPixbuf*create_pixbuf(const gchar*filename) {
  GdkPixbuf*pixbuf;
  GError*error = NULL;
  pixbuf = gdk_pixbuf_new_from_file(filename, &error);
  if(!pixbuf) {
    fprintf(stderr,"\n%s\n", error->message);
    g_error_free(error);
  }
  return pixbuf;
}

#define NGON struct ngon
NGON {
  double Cx,Cy, r;
  int sides, selected;
  char c;
};

GRand*random_numbers = NULL;

#define R 20
#define TAU (2*M_PI)	   /* http://laughingsquid.com/pi-is-wrong/ */
#define OFFSET_X (1+sin(TAU/12))
#define OFFSET_Y (cos(TAU/12))
#define ODD(A) ((A)&1)

static void initialize_hexagons(NGON*hs,size_t n) {
  NGON*h;
  gint i,broken;
  GQueue*shuffler = g_queue_new();
  if (NULL == shuffler) {
    fputs("\ncannot allocate shuffling queue.  quitting!\n",stderr);
    exit(EXIT_FAILURE);
  }
  /* randomize characters by stuffing them onto a double end queue
     and popping them off from random positions */
  if ((broken = (NULL == random_numbers)))
    random_numbers = g_rand_new();
  for (i = 'A'; i <= 'Z'; ++i)
    g_queue_push_head(shuffler,GINT_TO_POINTER(i));
  memset(hs,0,n*(sizeof(NGON)));
  hs[n-1].sides = -1;		/* assign the sentinel */
  for (h = hs; !h->sides; ++h) {
    int div = (h-hs)/4, mod = (h-hs)%4;
    h->sides = 6;
    h->c = GPOINTER_TO_INT(
	     g_queue_pop_nth(
	       shuffler,
	       g_rand_int_range(
		 random_numbers,
		 (gint32)0,
		 (gint32)g_queue_get_length(shuffler))));
    fputc(h->c,stderr);
    h->r = R;
    h->Cx = R*(2+div*OFFSET_X), h->Cy = R*(2*(1+mod*OFFSET_Y)+ODD(div)*OFFSET_Y);
    fprintf(stderr,"(%g,%g)\n",h->Cx,h->Cy);
  }
  fputc('\n',stderr);
  g_queue_free(shuffler);
  if (broken)
    g_rand_free(random_numbers);
}

static void add_loop(cairo_t*cr,NGON*hs,int select) {
  NGON*h;
  double r,Cx,Cy,x,y;
  int i, sides;
  for (h = hs; 0 < (sides = h->sides); ++h)
    if ((select && h->selected) || (select == h->selected)) {
      r = h->r, Cx = h->Cx, Cy = h->Cy;
      i = 0;
      x = Cx+r*cos(TAU*i/sides), y = Cy+r*sin(TAU*i/sides), cairo_move_to(cr,x,y);
      for (i = 1; i < sides; ++i)
        x = Cx+r*cos(TAU*i/sides), y = Cy+r*sin(TAU*i/sides), cairo_line_to(cr,x,y);
      cairo_close_path(cr);
    }
}

static int make_labels(cairo_t*cr,NGON*hs,int select) {
  NGON*h;
  int i = 0;
  char text[2];
  text[1] = 0;
  for (h = hs; 0 < h->sides; ++h)
    if ((select && h->selected) || (select == h->selected))
      /* yuck, need to measure the font.  Better to use pango_cairo */
      *text = h->c, cairo_move_to(cr,h->Cx,h->Cy), cairo_show_text(cr,text), ++i;
  return i;
}

static int archive(int a) {
  static GQueue*q = NULL;
  if ((NULL == q) && (NULL == (q = g_queue_new()))) {
    fputs("\ncannot allocate archival queue.  quitting!\n",stderr);
    exit(EXIT_FAILURE);
  }
  if (a < -1)			/* reset */
    return g_queue_free(q), q = NULL, 0;
  if (-1 == a)			/* pop off tail */
    return g_queue_is_empty(q) ? 0 : GPOINTER_TO_INT(g_queue_pop_tail(q));
  if (!a)			/* peek most recent entry */
    return g_queue_is_empty(q) ? 0 : GPOINTER_TO_INT(g_queue_peek_head(q));
  g_queue_push_head(q,GINT_TO_POINTER(a)); /* store */
  return a;
}

/* to do: use appropriate sizing, use the cairo transformation matrix */
static gboolean draw(GtkWidget*widget,cairo_t*cr,gpointer data) {

  /* unselected fill in yellow */
  cairo_set_source_rgba(cr,0.8,0.8,0,1),
  add_loop(cr,(NGON*)data,0);
  cairo_fill(cr);

  /* selected fill, purple */
  cairo_set_source_rgba(cr,0.8,0,0.8,1);
  add_loop(cr,(NGON*)data,1);
  cairo_fill_preserve(cr);

  /* all outlines gray, background shows through, fun fun! */
  cairo_set_line_width (cr, 3.0);
  cairo_set_source_rgba(cr,0.7,0.7,0.7,0.7);
  add_loop(cr,(NGON*)data,0);
  cairo_stroke(cr);

  /* select labels */
  cairo_set_source_rgba(cr,0,1,0,1);
  make_labels(cr,(NGON*)data,1);
  cairo_stroke(cr);

  /* unselected labels */
  cairo_set_source_rgba(cr,1,0,0,1);
  /* to do: clean up this exit code */
  if (!make_labels(cr,(NGON*)data,0)) {
    int c;
    putchar('\n');
    while ((c = archive(-1)))
      putchar(c);
    puts("\nfinished");
    archive(-2);
    exit(EXIT_SUCCESS);
  }
  cairo_stroke(cr);

  return TRUE;
}

/*the widget is a GtkDrawingArea*/
static gboolean button_press_event(GtkWidget*widget,const GdkEvent*event,gpointer data) {
  NGON*h,*hs = (NGON*)data;
  gdouble x_win, y_win;
  if (!gdk_event_get_coords(event,&x_win,&y_win))
    fputs("\nBUTTON, gdk_event_get_coords(event,&x_win,&y_win)) failed\n",stderr);
  else {
    fprintf(stderr,"x_win=%g y_win=%g\n",(double)x_win,(double)y_win);
    for (h = hs; 0 < h->sides; ++h) /* detection algorithm: */
      /* if mouse click within inner radius of hexagon */
      /* Much easier than all in-order cross products have same sign test! */
      if ((pow((x_win-h->Cx),2)+pow((y_win-h->Cy),2)) < pow((h->r*cos(TAU/(180/h->sides))),2)) {
	++h->selected;
	archive(h->c);
	/* discovery: gdk_window_invalidate_region with NULL second argument does not work */
	gdk_window_invalidate_rect(gtk_widget_get_window(widget),(const GdkRectangle*)NULL,TRUE);
	break;
      }
  }
  return TRUE;
}

static gboolean key_press_event(GtkWidget*widget,const GdkEvent*event,gpointer data) {
  NGON*h,*hs = (NGON*)data;
  guint keyval;
  int unicode;
  if (!gdk_event_get_keyval(event,&keyval))
    fputs("\nKEY!  gdk_event_get_keyval(event,&keyval)) failed.\n",stderr);
  else {
    unicode = (int)gdk_keyval_to_unicode(gdk_keyval_to_upper(keyval));
    fprintf(stderr,"key with unicode value: %d\n",unicode);
    for (h = hs; 0 < h->sides; ++h) /* look for a matching character associated with a hexagon */
      if (h->c == unicode) {
	++(h->selected);
	archive(h->c);
	/* discovery: gdk_window_invalidate_region with NULL second argument does not work */
	gdk_window_invalidate_rect(gtk_widget_get_window(widget),(const GdkRectangle*)NULL,TRUE);
	break;
      }
  }
  return TRUE;
}

int main(int argc,char*argv[]) {
  GtkWidget *window, *vbox, /* *label, */ *drawing_area;
  NGON ngons[21];	   /* sentinal has negative number of sides */

  /* discovery: gtk_init removes gtk debug flags, such as --gtk-debug=all */
  /*   also calls gdk_init which handles --display and --screen or other X11 communication issues */
  gtk_init(&argc, &argv);

  /* GTK VERSION 3.2.0 */
  fprintf(stderr,"GTK VERSION %d.%d.%d\n",GTK_MAJOR_VERSION,GTK_MINOR_VERSION,GTK_MICRO_VERSION);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  /* discovery: to make window transparent I have to use the alpha channel correctly */

  /* discovery: GTK_WINDOW(GtkWidget*) casts the widget to window */
  /* discovery: window in the function name?  use GTK_WINDOW.  g_ in function name?  use G_OBJECT */
  gtk_window_set_title(GTK_WINDOW(window), "Rosetta Code Honeycomb, C with GTK");
  gtk_window_set_default_size(GTK_WINDOW(window), 308, 308+12+8); /* XxY */
  /* discovery: making the window vanish does not stop the program */
  /* discovery: NULL is placeholder for extra data sent to the callback */
  g_signal_connect_swapped(G_OBJECT(window),"destroy",G_CALLBACK(gtk_main_quit),NULL);

  /* I created /tmp/favicon.ico from http://rosettacode.org/favicon.ico */
  /* Your window manager could use the icon, if it exists, and you fix the file name */
  gtk_window_set_icon(GTK_WINDOW(window),create_pixbuf("/tmp/favicon.ico"));

  vbox = gtk_vbox_new(TRUE,1);
  gtk_container_add(GTK_CONTAINER(window),vbox);

  /* to do: fix the label widget */
  /* I did not learn to control multiple box packing, and I was */
  /* too lazy to make the label widget accessible.  Plan was to */
  /* insert the most recent character using "peek" option of the archive */
#if 0
  label = gtk_label_new("None Selected");
  gtk_widget_set_size_request(label,308,20);
  gtk_box_pack_end(GTK_BOX(vbox),label,FALSE,TRUE,4);
#endif

  drawing_area = gtk_drawing_area_new();
  gtk_widget_set_events(drawing_area,GDK_BUTTON_PRESS_MASK|GDK_KEY_PRESS_MASK|GDK_EXPOSURE_MASK);

  random_numbers = g_rand_new();
  initialize_hexagons(ngons,G_N_ELEMENTS(ngons));
  /* Discovery: expose_event changed to draw signal.  We no longer need configure-event */
  g_signal_connect(G_OBJECT(drawing_area),"draw",G_CALLBACK(draw),(gpointer)ngons);

  g_signal_connect(G_OBJECT(drawing_area),"button-press-event",G_CALLBACK(button_press_event),(gpointer)ngons);
  g_signal_connect(G_OBJECT(drawing_area),"key-press-event",G_CALLBACK(key_press_event),(gpointer)ngons);
  gtk_widget_set_size_request(drawing_area, 308, 308); /* XxY */
  gtk_box_pack_start(GTK_BOX(vbox),drawing_area,TRUE,TRUE,4);

  /* Discovery: must allow focus to receive keyboard events */
  gtk_widget_set_can_focus(drawing_area,TRUE);

  /* Discovery: can set show for individual widgets or use show_all */
  gtk_widget_show_all(window);
  gtk_main();
  g_rand_free(random_numbers);
  return EXIT_SUCCESS;
}

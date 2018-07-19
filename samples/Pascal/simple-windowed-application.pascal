Program SimpleWindowApplication;

uses
  SysUtils,
  glib2,
  Gtk2;

const
  clickme = 'Click Me';
  MAXLEN = 64;

var
  counter: integer = 0;

procedure clickedme(o: PGtkButton; d: pointer); cdecl;
  var
    nt: Pchar;
    l: PGtkLabel;
  begin
    l := Gtk_LABEL(d);
    inc(counter);
    nt := Pchar('You clicked me ' + inttostr(counter) + ' times');
    Gtk_label_set_text(l, nt);
  end;

var
  win:     PGtkWindow;
  button:  PGtkButton;
  Mylabel: PGtkLabel;
  vbox:    PGtkVBox;

begin
  Gtk_init(@argc, @argv);
  win := PGtkWindow(Gtk_window_new(Gtk_WINDOW_TOPLEVEL));
  Gtk_window_set_title(win, clickme);
  button := PGtkButton(Gtk_button_new_with_label(clickme));
  Mylabel := PGtkLabel(Gtk_label_new('There have been no clicks yet'));
  Gtk_label_set_single_line_mode(Mylabel, TRUE);
  vbox := PGtkVBox(Gtk_vbox_new(TRUE, 1));
  Gtk_container_add(Gtk_CONTAINER(vbox), Gtk_WIDGET(Mylabel));
  Gtk_container_add(Gtk_CONTAINER(vbox), Gtk_WIDGET(button));
  Gtk_container_add(Gtk_CONTAINER(win), Gtk_WIDGET(vbox));
  g_signal_connect(G_OBJECT(win), 'delete-event', TGCallBack(@Gtk_main_quit), NULL);
  g_signal_connect(G_OBJECT(button), 'clicked', TGCallBack(@clickedme), Mylabel);
  Gtk_widget_show_all(Gtk_WIDGET(win));
  Gtk_main();
end.

// Part of linguist/samples

import Shapes;
import Cpp library "<gtkmm.h>";

alias Optional = Core.Optional;

fn Run(argc: i32, argv: Optional(char*)*) -> i32 {
  var app: auto = Cpp.Gtk.Application.create();

  var rect: Shapes.Rectangle = { .Width = 500, .Height = 1250 };

  var window: Cpp.Gtk.Window;
  window.set_title("Window Name");
  window.set_default_size(rect.Width unsafe as i32, rect.Height unsafe as i32);

  return app.make_window_and_run(Cpp.Gtk.Window, window, argc, argv);
}

import gtk.MainWindow, gtk.Label, gtk.Main;

class GoodbyeWorld : MainWindow {
    this() {
        super("GtkD");
        add(new Label("Goodbye World"));
        showAll();
    }
}

void main(string[] args) {
    Main.init(args);
    new GoodbyeWorld();
    Main.run();
}

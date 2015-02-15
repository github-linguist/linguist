if (!class_exists('gtk'))
{
    die("Please load the php-gtk2 module in your php.ini\r\n");
}

$wnd = new GtkWindow();
$wnd->set_title('Goodbye world');
$wnd->connect_simple('destroy', array('gtk', 'main_quit'));

$lblHello = new GtkLabel("Goodbye, World!");
$wnd->add($lblHello);

$wnd->show_all();
Gtk::main();

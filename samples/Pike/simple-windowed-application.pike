GTK2.Widget mainwindow,clickcnt,clicker;
int clicks;

void click()
{
        clickcnt->set_text("Clicks: "+(++clicks));
}

int main()
{
        GTK2.setup_gtk();
        mainwindow=GTK2.Window(GTK2.WindowToplevel);
        mainwindow->set_title("Click counter");
        mainwindow->add(GTK2.Vbox(0,10)
                ->add(clickcnt=GTK2.Label("There have been no clicks yet"))
                ->add(clicker=GTK2.Button("Click me"))
        )->show_all();
        mainwindow->signal_connect("delete_event",lambda() {exit(0);});
        clicker->signal_connect("clicked",click);
        return -1;
}

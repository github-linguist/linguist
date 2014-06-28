import "gtk" as gtk
import "io" as io
import "mgcollections" as collections
import "button_factory" as button_factory
import "dialog_factory" as dialog_factory
import "syntax_highlighter" as highlighter
import "auto_completer" as aComp

//TODO

// Autocomplete typing

// FileChooser
// Themes

// Details for the Top Level Window
def window = gtk.window(gtk.GTK_WINDOW_TOPLEVEL)
window.title := "Grace"
window.set_default_size(700, 700)
// -------------

// Placeholder for the console window that can be popped out
// of the main window
var popped := gtk.window(gtk.GTK_WINDOW_TOPLEVEL)

// Initialise the Boxes
def mBox = gtk.box(gtk.GTK_ORIENTATION_VERTICAL, 2)
def buttonBox = gtk.box(gtk.GTK_ORIENTATION_HORIZONTAL, 2)
var consoleButtons := gtk.box(gtk.GTK_ORIENTATION_HORIZONTAL, 3)
var consoleBox := gtk.box(gtk.GTK_ORIENTATION_VERTICAL, 2)
var editorBox := gtk.box(gtk.GTK_ORIENTATION_VERTICAL, 2)
var splitPane := gtk.paned(gtk.GTK_ORIENTATION_VERTICAL, 2)
def menuBox = gtk.box(gtk.GTK_ORIENTATION_HORIZONTAL, 4)
// -------------

// Initialise the buttons
def runButton = button_factory.make("run")
var clearButton := button_factory.make("clear")
var outButton := button_factory.make("out")
var errorButton := button_factory.make("error")
var popButton := button_factory.make("pop")
def newButton = button_factory.make("new")
def openButton = button_factory.make("open")
def saveButton = button_factory.make("save")
def saveAsButton = button_factory.make("saveAs")
def closeButton = button_factory.make("close")
// -------------

// Details for the default text editor and scrolled window
var tEdit := gtk.text_view
tEdit.set_size_request(700, 400)

var scrolled_main := gtk.scrolled_window
scrolled_main.set_size_request(700, 400)
scrolled_main.add(tEdit)
// -------------

// Widget that allows multiple files to be edited (tabs)
var notebook := gtk.notebook
notebook.scrollable := true
// -------------

// Maps for holding the text_views and scrolled_windows
var editor_map := collections.map.new
editor_map.put(0, tEdit)
var scrolled_map := collections.map.new
scrolled_map.put(0, scrolled_main)

// -------------

// Class that manages the syntax highlighting (This needs to be passed around otherwise
// the text_tag table gets confused, ie there can only be one)
def lighter = highlighter.Syntax_Highlighter.new(notebook, editor_map)
tEdit.buffer.on "changed" do {
    lighter.highlightLine
}

// Class that manages any auto completion that is required
def completer =  aComp.Auto_Completer.new(window, notebook, editor_map)

// Utility methods
// -------------

method deleteCompileFiles(page_num : Number) {
    def cur_scrolled = scrolled_map.get(page_num)
    var filename := notebook.get_tab_label_text(cur_scrolled)
    filename := filename.substringFrom(0)to(filename.size - 7) //Removes .grace extension

    io.system("rm -f files/" ++ filename)
    io.system("rm -f files/" ++ filename ++ ".c")
    io.system("rm -f files/" ++ filename ++ ".gcn")
    io.system("rm -f files/" ++ filename ++ ".gct")
}

// -------------



var currentConsole := "output"      // Which console is being shown
var out := false


var outText := ""
var errorText := ""



// Give actions to the buttons
// -------------

runButton.on "clicked" do {
    clearConsoles()

    // Get the details for the current page selected
    def cur_page_num = notebook.current_page
    def cur_page = editor_map.get(cur_page_num)
    def cur_scrolled = scrolled_map.get(cur_page_num)
    def cur_page_label = notebook.get_tab_label_text(cur_scrolled)

    // Initialise text iterators
    def sIter = gtk.text_iter
    def eIter = gtk.text_iter

    // Set one at the beggining and one at the end of the text
    cur_page.buffer.get_iter_at_offset(sIter, 0)
    cur_page.buffer.get_iter_at_offset(eIter, -1)

    // Get the text between the text iterators
    def text = cur_page.buffer.get_text(sIter, eIter, true)

    // Save the text to the file (in case the user hasn't already saved it)
    def file = io.open("files/" ++ cur_page_label, "w")
    file.write(text)
    file.close

    // Run the program and pipe the output and errors into files to be read
    io.system("../minigrace/minigrace " ++ "files/" ++ cur_page_label ++ " > output.txt 2> error.txt")
    def outputFile = io.open("output.txt", "r")
    def errorFile = io.open("error.txt", "r")
    outText := outputFile.read
    errorText := errorFile.read

    io.system("rm -f output.txt error.txt")

    var switched := false

    // Change the console to output if there is output text
    if((outText.size > 0) && (currentConsole != "output")) then {
        switch_to_output()
        switched := true
    }
    // Change the console to errors if there were errors
    if((errorText.size > 0) && (currentConsole != "errors")) then {
        switch_to_errors()
        switched := true
    }

    // Remember to populate the console if it wasn't switched
    if(!switched) then {
        populateConsoles
    }
}

clearButton.on "clicked" do {
    clearConsoles()
}

outButton.on "clicked" do {
    switch_to_output()
}

errorButton.on "clicked" do {
    switch_to_errors()
}

popButton.on "clicked" do {
    if(out) then {
        popIn()
    } else {
        popOut()
    }
}

// Gives a dialog to let the user create a new file to edit
newButton.on "clicked" do {
    def new_window_class = dialog_factory.new.new(notebook, editor_map, scrolled_map, lighter)

    def new_window = new_window_class.window()
    new_window.show_all
}

// Gives a dialog that lets the user open a file to edit
openButton.on "clicked" do {
    def open_window_class = dialog_factory.open.new(notebook, editor_map, scrolled_map, lighter)

    def open_window = open_window_class.window()
    open_window.show_all
}

// Saves the current file (if the name is Untitled.grace it will ask for a new name)
saveButton.on "clicked" do {
    def cur_page_num = notebook.current_page
    def cur_page = editor_map.get(cur_page_num)
    def cur_scrolled = scrolled_map.get(cur_page_num)
    def cur_page_label = notebook.get_tab_label_text(cur_scrolled)

    if(cur_page_label == "Untitled.grace") then {
        def saveAs_window_class = dialog_factory.save.new(notebook, editor_map, scrolled_map, true)

        def saveAs_window = saveAs_window_class.window()
        saveAs_window.show_all
    } else {
        // Initialise text iterators
        def sIter = gtk.text_iter
        def eIter = gtk.text_iter

        // Set one at the beggining and one at the end of the text
        cur_page.buffer.get_iter_at_offset(sIter, 0)
        cur_page.buffer.get_iter_at_offset(eIter, -1)

        // Get the text between the text iterators
        def text = cur_page.buffer.get_text(sIter, eIter, true)

        // Save the file
        def file = io.open("files/" ++ cur_page_label, "w")
        file.write(text)
        file.close
    }

}

// Gives a dialog that lets the user save the file with a new name
saveAsButton.on "clicked" do {
    def saveAs_window_class = dialog_factory.save.new(notebook, editor_map, scrolled_map, false)

    def saveAs_window = saveAs_window_class.window()
    saveAs_window.show_all
}

// This will close a tab on the notebook
// It also "removes" the page from the map,
// by creating a new temporary map and putting all but
// the removed page in.
closeButton.on "clicked" do {
    def page_num = notebook.current_page
    def num_pages = notebook.n_pages

    if(num_pages > 1) then {
        deleteCompileFiles(page_num)

        def e_map = collections.map.new
        def s_map = collections.map.new

        // Copy every page up to the current page into the new maps
        var x := 0
        while {x < page_num} do {
            var eValue := editor_map.get(x)
            var sValue := scrolled_map.get(x)
            e_map.put(x, eValue)
            s_map.put(x, sValue)

            x := x + 1
        }

        // Copy every page after the current page into the new map (shifted one down)
        x := page_num + 1
        while {x < num_pages} do {
            var eValue := editor_map.get(x)
            var sValue := scrolled_map.get(x)
            e_map.put((x - 1), eValue)
            s_map.put((x - 1), sValue)

            x := x + 1
        }

        editor_map := e_map
        scrolled_map := s_map
        notebook.remove_page(page_num)

        notebook.show_all
    }

}
// -------------






// Consoles:
// -------------

var outConsole := gtk.text_view
var outScroll := gtk.scrolled_window
var errorConsole := gtk.text_view
var errorScroll := gtk.scrolled_window
var errorTag := errorConsole.buffer.create_tag("fixed", "foreground", "red")


// Creates a new output console
method createOut {
    outConsole := gtk.text_view
    outScroll := gtk.scrolled_window
    outScroll.add(outConsole)
    if(out) then {
        outConsole.set_size_request(400, 400)
        outScroll.set_size_request(400, 400)
    } else {
        outConsole.set_size_request(700, 200)
        outScroll.set_size_request(700, 200)
    }
    outConsole.editable := false
    outConsole.buffer.set_text("[Output]:", -1)
}
createOut()

// Creates a new error console
method createError {
    errorConsole := gtk.text_view
    errorScroll := gtk.scrolled_window
    errorScroll.add(errorConsole)
    if(out) then {
        errorConsole.set_size_request(400, 400)
        errorScroll.set_size_request(400, 400)
    } else {
        errorConsole.set_size_request(700, 200)
        errorScroll.set_size_request(700, 200)
    }
    errorConsole.editable := false
    errorConsole.buffer.set_text("[Errors]:", -1)
    errorTag := errorConsole.buffer.create_tag("fixed", "foreground", "red")
}
createError()

// Switches the console being shown to be output. This requires
// the output console to be remade as it would have been destroyed when
// it was switched previously
method switch_to_output {
    if(currentConsole != "output") then {
        currentConsole := "output"
        consoleBox.remove(errorScroll)     // This destroys the errorConsole

        createOut()

        consoleBox.add(outScroll)

        populateConsoles()
        if(out) then {
            popped.show_all
        } else {
            window.show_all
        }
    }
}

// Switches the console being shown to be errors. This requires
// the error console to be remade as it would have been destroyed when
// it was switched previously
method switch_to_errors {
    if(currentConsole != "errors") then {
        currentConsole := "errors"
        consoleBox.remove(outScroll)       // This destroys the outConsole

        createError()

        consoleBox.add(errorScroll)

        populateConsoles()
        if(out) then {
            popped.show_all
        } else {
            window.show_all
        }
    }
}

// If there is text to be put into the consoles this will add it
method populateConsoles {
    if((outText.size > 0) && (currentConsole == "output")) then {
        outConsole.buffer.set_text(outText, -1)
    }
    if((errorText.size > 0) && (currentConsole == "errors")) then {
        def sIter = gtk.text_iter
        def eIter = gtk.text_iter

        errorConsole.buffer.set_text(errorText, -1)
        errorConsole.buffer.get_iter_at_offset(sIter, 0)
        errorConsole.buffer.get_iter_at_offset(eIter, -1)
        errorConsole.buffer.apply_tag(errorTag, sIter, eIter)
    }
}

method clearConsoles {
    if(currentConsole == "output") then {
        outConsole.buffer.set_text("[Output]:", -1)
        outText := ""
    }
    if(currentConsole == "errors") then {
        errorConsole.buffer.set_text("[Errors]:", -1)
        errorText := ""
    }
}


// Identical as the popIn method, but can be connected to the window's destroy button
def popInBlock = {
    consoleBox.reparent(splitPane)
    popButton.label := "Pop Out"

    if(currentConsole == "output") then {
        outConsole.set_size_request(700, 200)
        outScroll.set_size_request(700, 200)
    }
    if(currentConsole == "errors") then {
     errorConsole.set_size_request(700, 200)
     errorScroll.set_size_request(700, 200)
    }

    def cur_page_num = notebook.current_page
    def cur_scrolled = scrolled_map.get(cur_page_num)
    def cur_page = editor_map.get(cur_page_num)

    cur_page.set_size_request(700, 400)
    cur_scrolled.set_size_request(700, 400)

    out := false
    popped.visible := false
}


// This pops the console out into a separate window
method popOut {
    popped := gtk.window(gtk.GTK_WINDOW_TOPLEVEL)

    consoleBox.reparent(popped)
    popButton.label := "Pop In"

    if(currentConsole == "output") then {
        outConsole.set_size_request(400, 400)
        outScroll.set_size_request(400, 400)
    }
    if(currentConsole == "errors") then {
        errorConsole.set_size_request(400, 400)
        errorScroll.set_size_request(400, 400)
    }

    def cur_page_num = notebook.current_page
    def cur_scrolled = scrolled_map.get(cur_page_num)
    def cur_page = editor_map.get(cur_page_num)

    cur_page.set_size_request(700, 580)
    cur_scrolled.set_size_request(700, 580)

    out := true
    popped.visible := true
    popped.connect("destroy", popInBlock)
    popped.show_all

}

// Puts the console back into the main window
method popIn {
    consoleBox.reparent(splitPane)
    popButton.label := "Pop Out"

    if(currentConsole == "output") then {
        outConsole.set_size_request(700, 200)
        outScroll.set_size_request(700, 200)
    }
    if(currentConsole == "errors") then {
        errorConsole.set_size_request(700, 200)
        errorScroll.set_size_request(700, 200)
    }

    def cur_page_num = notebook.current_page
    def cur_scrolled = scrolled_map.get(cur_page_num)
    def cur_page = editor_map.get(cur_page_num)

    cur_page.set_size_request(700, 400)
    cur_scrolled.set_size_request(700, 400)

    out := false
    popped.visible := false
}

clearConsoles()
// -------------






// Patch everything together

var hSeparator1 := gtk.separator(gtk.GTK_ORIENTATION_HORIZONTAL)
var hSeparator2 := gtk.separator(gtk.GTK_ORIENTATION_HORIZONTAL)

menuBox.add(newButton)
menuBox.add(openButton)
menuBox.add(saveButton)
menuBox.add(saveAsButton)
buttonBox.add(runButton)
buttonBox.add(closeButton)

consoleButtons.add(outButton)
consoleButtons.add(errorButton)
consoleButtons.add(clearButton)
consoleButtons.add(popButton)

consoleBox.add(hSeparator1)
consoleBox.add(consoleButtons)
consoleBox.add(outScroll)

editorBox.add(hSeparator2)
notebook.add(scrolled_main)
notebook.set_tab_label_text(scrolled_main, "Untitled.grace")
editorBox.add(notebook)

splitPane.add1(editorBox)
splitPane.add2(consoleBox)

mBox.add(menuBox)
mBox.add(buttonBox)
mBox.add(splitPane)

window.add(mBox)

def exit = {
    var x := 0
    while {x < notebook.n_pages} do {
        deleteCompileFiles(x)

        x := x + 1
    }

    // Delete the compile files of the IDE
    io.system("rm -f Grace_IDE.gct Grace_IDE.c Grace_IDE.gcn")
    io.system("rm -f scanner.gct scanner.c scanner.gcn")
    io.system("rm -f syntax_highlighter.gct syntax_highlighter.c syntax_highlighter.gcn")
    io.system("rm -f syntax_colors.gct syntax_colors.c syntax_colors.gcn")
    io.system("rm -f button_factory.gct button_factory.c button_factory.gcn")
    io.system("rm -f dialog_factory.gct dialog_factory.c dialog_factory.gcn")
    io.system("rm -f auto_completer.gct auto_completer.c auto_completer.gcn")

    print "Grace IDE Closed Successfully"
    gtk.main_quit
}

window.connect("destroy", exit)
window.show_all

gtk.main
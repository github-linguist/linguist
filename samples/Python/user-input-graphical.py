import Tkinter,tkSimpleDialog

root = Tkinter.Tk()
root.withdraw()

number = tkSimpleDialog.askinteger("Integer", "Enter a Number")
string = tkSimpleDialog.askstring("String", "Enter a String")

from tkinter import *
import tkinter.messagebox

def maximise():
	"""get screenwidth and screenheight, and resize window to that size.
	Also move to 0,0"""
	root.geometry("{}x{}+{}+{}".format(root.winfo_screenwidth(), root.winfo_screenheight(), 0, 0))
	
def minimise():
	"""Iconify window to the taskbar. When reopened, the window is
	unfortunately restored to its original state, NOT its most recent state."""
	root.iconify()
	
def delete():
	"""create a modal dialog. If answer is "OK", quit the program"""
	if tkinter.messagebox.askokcancel("OK/Cancel","Are you sure?"):
		root.quit()
	
root = Tk()

mx=Button(root,text="maximise",command=maximise)
mx.grid()
mx.bind(maximise)

mn=Button(root,text="minimise",command=minimise)
mn.grid()
mn.bind(minimise)

#catch exit events, including "X" on title bar.
root.protocol("WM_DELETE_WINDOW",delete)

mainloop()

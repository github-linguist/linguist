// The MIT License (MIT)

// Copyright (c) 2016 Sébastien Kirche

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Source: https://github.com/sebkirche/libexport/blob/c2459a5399ff87f29344b3b0472b041ebb6f9246/sources/115/w_export.srw

HA$PBExportHeader$w_export.srw
forward
global type w_export from window
end type
type st_2 from statictext within w_export
end type
type sle_destdir from singlelineedit within w_export
end type
type cb_browse_dest from commandbutton within w_export
end type
type cb_export_some from commandbutton within w_export
end type
type cb_export_all from commandbutton within w_export
end type
type dw_objects from datawindow within w_export
end type
type cb_browse from commandbutton within w_export
end type
type sle_pbl from singlelineedit within w_export
end type
type st_1 from statictext within w_export
end type
end forward

global type w_export from window
integer width = 3209
integer height = 1600
boolean titlebar = true
string title = "Export"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_2 st_2
sle_destdir sle_destdir
cb_browse_dest cb_browse_dest
cb_export_some cb_export_some
cb_export_all cb_export_all
dw_objects dw_objects
cb_browse cb_browse
sle_pbl sle_pbl
st_1 st_1
end type
global w_export w_export

forward prototypes
public subroutine get_objects (string as_pbl)
public function boolean export_object (string as_lib, string as_object, string as_type, string as_comment)
public function string get_object_suffix (string as_type)
public function libexporttype get_object_libtype (string as_type)
public subroutine export_object_at_row (long al_row, string as_lib)
end prototypes

public subroutine get_objects (string as_pbl);
if not fileexists(as_pbl) then return

int i, p = 1
string ls_entries, ls_entry
ls_entries = LibraryDirectoryEx(as_pbl, DirAll!)
debug_message('get_objects()', ls_entries)

dw_objects.reset()
dw_objects.importstring(ls_entries)

end subroutine

public function boolean export_object (string as_lib, string as_object, string as_type, string as_comment);
LibExportType l_ot
string ls_syntax, ls_filename, ls_dir
int li_file

ls_dir = sle_destdir.text
l_ot = get_object_libtype(as_type)
ls_syntax = libraryexport(as_lib, as_object, l_ot)

ls_filename = as_object + '.' + get_object_suffix(as_type)
li_file = FileOpen(ls_dir + '\' + ls_filename, streammode!, write!, LockReadWrite!, Replace!, EncodingUTF16LE!)
if li_file = -1 then return false

FileWrite(li_file, "$PBExportHeader$" + ls_filename + "~r~n")
if as_comment <> "" then
	FileWrite(li_file, "$PBExportComments$" + as_comment + "~r~n")
end if
FileWrite(li_file, ls_syntax)
FileClose(li_file)

return true

end function

public function string get_object_suffix (string as_type);
string ls_suf = ""

choose case as_type
	case 'Application'; ls_suf = 'sra'
	case 'DataWindow';  ls_suf = 'srd'
	case 'Function';    ls_suf = 'srf'
	case 'Menu';        ls_suf = 'srm'
	case 'Pipeline';    ls_suf = 'srp'
	case 'Project';     ls_suf = 'srj'
	case 'Query';       ls_suf = 'srq'
	case 'Structure';   ls_suf = 'srs'
	case 'UserObject';  ls_suf = 'sru'
	case 'Window';      ls_suf = 'srw'
end choose

return ls_suf

end function

public function libexporttype get_object_libtype (string as_type);
libexporttype l_ot

choose case as_type
	case 'Application'; l_ot = ExportApplication!
	case 'DataWindow';  l_ot = ExportDataWindow!
	case 'Function';    l_ot = ExportFunction!
	case 'Menu';        l_ot = ExportMenu!
	case 'Pipeline';    l_ot = ExportPipeline!
	case 'Project';     l_ot = ExportProject!
	case 'Query';       l_ot = ExportQuery!
	case 'Structure';   l_ot = ExportStructure!
	case 'UserObject';  l_ot = ExportUserObject!
	case 'Window';      l_ot = ExportWindow!
end choose

return l_ot

end function

public subroutine export_object_at_row (long al_row, string as_lib);
string ls_obj, ls_type, ls_comment

ls_obj = dw_objects.getitemstring(al_row, "obj_name")
ls_type = dw_objects.getitemstring(al_row, "obj_type")
ls_comment = dw_objects.getitemstring(al_row, "comment")
export_object(as_lib, ls_obj, ls_type, ls_comment)

end subroutine

on w_export.create
this.st_2=create st_2
this.sle_destdir=create sle_destdir
this.cb_browse_dest=create cb_browse_dest
this.cb_export_some=create cb_export_some
this.cb_export_all=create cb_export_all
this.dw_objects=create dw_objects
this.cb_browse=create cb_browse
this.sle_pbl=create sle_pbl
this.st_1=create st_1
this.Control[]={this.st_2,&
this.sle_destdir,&
this.cb_browse_dest,&
this.cb_export_some,&
this.cb_export_all,&
this.dw_objects,&
this.cb_browse,&
this.sle_pbl,&
this.st_1}
end on

on w_export.destroy
destroy(this.st_2)
destroy(this.sle_destdir)
destroy(this.cb_browse_dest)
destroy(this.cb_export_some)
destroy(this.cb_export_all)
destroy(this.dw_objects)
destroy(this.cb_browse)
destroy(this.sle_pbl)
destroy(this.st_1)
end on

type st_2 from statictext within w_export
integer x = 32
integer y = 1208
integer width = 251
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
string text = "Export into"
boolean focusrectangle = false
end type

type sle_destdir from singlelineedit within w_export
integer x = 325
integer y = 1204
integer width = 2619
integer height = 80
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
end type

type cb_browse_dest from commandbutton within w_export
integer x = 2985
integer y = 1204
integer width = 169
integer height = 80
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;
string ls_dir

ls_dir = getfolder("Select the directory where you want to export")
if ls_dir <> "" then
	sle_destdir.text = ls_dir
end if

end event

type cb_export_some from commandbutton within w_export
integer x = 398
integer y = 1344
integer width = 457
integer height = 100
integer taborder = 50
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Export selected"
end type

event clicked;
string ls_lib
int r

ls_lib = sle_pbl.text
if not fileexists(ls_lib) then return

r = dw_objects.getselectedrow(0)
do while r > 0
	export_object_at_row(r, ls_lib)
	r = dw_objects.getselectedrow(r)
loop

end event

type cb_export_all from commandbutton within w_export
integer x = 37
integer y = 1344
integer width = 338
integer height = 100
integer taborder = 40
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Export all"
end type

event clicked;
string ls_lib
int i, li_max

ls_lib = sle_pbl.text
if not fileexists(ls_lib) then return

li_max = dw_objects.rowcount()
for i = 1 to li_max
	export_object_at_row(i, ls_lib)
next
end event

type dw_objects from datawindow within w_export
integer x = 32
integer y = 212
integer width = 2912
integer height = 876
integer taborder = 30
string title = "none"
string dataobject = "dw_objects"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;
boolean lb_sel

lb_sel = dw_objects.isselected(row)
dw_objects.selectrow(row, not lb_sel)

end event

type cb_browse from commandbutton within w_export
integer x = 2985
integer y = 64
integer width = 169
integer height = 80
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;
string ls_dir, ls_file, ls_path

if 1 = getfileopenname("Choose a library to export", ls_dir, ls_file, "pbl", "Libraries (*.PBL),*.PBL") then
	ls_path = ls_dir
	sle_pbl.text = ls_path
	get_objects(ls_path)
end if

end event

type sle_pbl from singlelineedit within w_export
integer x = 256
integer y = 64
integer width = 2688
integer height = 80
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_export
integer x = 32
integer y = 72
integer width = 215
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
string text = "Library"
boolean focusrectangle = false
end type


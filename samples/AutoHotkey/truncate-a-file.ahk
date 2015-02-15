truncFile("S:\Portables\AutoHotkey\My Scripts\Other_Codes\motion2.ahk", 1200)
return

truncFile(file, length_bytes){
	if !FileExist(file)
		msgbox, File doesn't exists.
	FileGetSize, fsize, % file, B
	if (length_bytes>fsize)
		msgbox, New truncated size more than current file size
	f := FileOpen(file, "rw")
	f.length := length_bytes
	f.close()
}

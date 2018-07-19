Debug FormatDate("%yyyy/%mm/%dd", GetFileDate("file.txt",#PB_Date_Modified))
  SetFileDate("file.txt",#PB_Date_Modified,Date(1987, 10, 23, 06, 43, 15))
Debug FormatDate("%yyyy/%mm/%dd - %hh:%ii:%ss", GetFileDate("file.txt",#PB_Date_Modified))

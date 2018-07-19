Sub Renamer()
    Dim f As FolderItem, r As FolderItem

    f = GetFolderItem("input.txt")
    'Changing a FolderItem's Name attribute renames the file or directory.
    If f.Exists Then f.Name = "output.txt"
    'Files and directories are handled almost identically in RB.
    f = GetFolderItem("docs")
    If f.Exists Then f.Name = "mydocs"

    'Jump through hoops to find the root directory.
    r = RootDir(GetFolderItem("."))

    f = r.Child("input.txt")
    'Renaming in a different directory identical to renaming in current directory.
    If f.Exists Then f.Name = "output.txt"
    f = r.Child("docs")
    If f.Exists Then f.Name = "mydocs"
End Sub

Function RootDir(what As FolderItem) As FolderItem
    'RB doesn't have an easy way to find the root of the current drive;
    'not an issue under *nix but troublesome under Windows.
    If what.Parent <> Nil Then  'Nil = no parent = root.
        Return RootDir(what.Parent) 'Recursive.
    Else
        Return what
    End If
End Function

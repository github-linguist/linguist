script "hideFolder"

command hideFolder pPath
    local tLastItem, tNewName
    set the itemdel to "/"

    if "\" is in pPath then replace "\" with "/" in pPath

    put pPath into tNewName
    put the item - 1 of pPath into tLastItem

    if platform() is "Win32" then
        set the hideConsoleWindows to true
        get shell( "ATTRIB +H " & quote & tNewName  & quote  )
        if it is not empty then
            answer error it
        end if
    else if the char 1 of tLastItem is not "." then
        put "." & tLastItem into the item - 1 of tNewName
        rename folder pPath to tNewName
    end if

end hideFolder



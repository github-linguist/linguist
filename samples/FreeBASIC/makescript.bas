#define NULL 0
#define FALSE 0
#define TRUE (-1)
#define STRINGIFY(s) #s

private sub fatalCantOpenFile(byref file as string)
    print "Error: Could not open file: '" + file + "'"
    end 1
end sub

private function strReplace _
    ( _
        byref text as string, _
        byref a as string, _
        byref b as string _
    ) as string

    static as string result
    static as string keep

    result = text

    dim as integer alen = len(a)
    dim as integer blen = len(b)

    dim as integer p = 0
    do
        p = instr(p + 1, result, a)
        if (p = 0) then
            exit do
        end if

        keep = mid(result, p + alen)
        result = left(result, p - 1)
        result += b
        result += keep
        p += blen - 1
    loop

    return result
end function

'' Searches backwards for the last '/' or '\'.
private function findFileName(byref path as string) as integer
    for i as integer = (len(path)-1) to 0 step -1
        dim as integer ch = path[i]
        if ((ch = asc("/")) or (ch = asc("\"))) then
            return i + 1
        end if
    next
    return 0
end function

private function pathStripFile(byref path as string) as string
    return left(path, findFileName(path))
end function

private function pathStripComponent(byref path as string) as string
    dim as string s = path

    '' Strip path div at the end
    dim as integer length = len(s)
    if (length > 0) then
        dim as integer ch = s[length-1]
        if ((ch = asc("/")) or (ch = asc("\"))) then
            s = left(s, len(s) - 1)
        end if
    end if

    '' Strip the last component
    return pathStripFile(s)
end function

private sub emitPath(byval o as integer, byref cmd as string, byref path as string)
    print #o, "    " + cmd + " ""$INSTDIR\" + path + """"
end sub

private sub emitRmDirs(byval o as integer, byref prevpath as string, byref path as string)
    '' RMDir foo\bar\baz
    '' RMDir foo\bar
    '' RMDir foo
    while ((len(prevpath) > 0) and (prevpath <> left(path, len(prevpath))))
        emitPath(o, "RMDir ", prevpath)
        prevpath = pathStripComponent(prevpath)
    wend
end sub

private sub emitInstallerFiles _
    ( _
        byref manifest as string, _
        byval o as integer, _
        byval install as integer _
    )

    dim as integer f = freefile()
    if (open(manifest, for input, as #f)) then
        fatalCantOpenFile(manifest)
    end if

    dim as string filename = ""
    dim as string path = ""
    dim as string prevpath = ""

    while (eof(f) = FALSE)
        line input #f, filename

        if (len(filename)) then
            '' Use backslashes for NSIS...
            filename = strReplace(filename, "/", "\")

            path = pathStripFile(filename)
            if (path <> prevpath) then
                if (install) then
                    emitPath(o, "SetOutPath", path)
                else
                    emitRmDirs(o, prevpath, path)
                end if
                prevpath = path
            end if

            if (install) then
                filename = "                   File """ + filename + """"
            else
                filename = "    Delete ""$INSTDIR\" + filename + """"
            end if

            print #o, filename
        end if
    wend

    if (install = FALSE) then
        emitRmDirs(o, prevpath, path)
    end if

    close #f
end sub

if (__FB_ARGC__ <> 4) then
    print "Usage: makescript manifest.lst template.nsi outputscript.nsi"
    end 1
end if

dim as string manifest  = *__FB_ARGV__[1]
dim as string inscript  = *__FB_ARGV__[2]
dim as string outscript = *__FB_ARGV__[3]

dim as integer i = freefile()
if (open(inscript, for input, as #i)) then
    fatalCantOpenFile(inscript)
end if

dim as integer o = freefile()
if (open(outscript, for output, as #o)) then
    fatalCantOpenFile(outscript)
end if

dim as string ln = ""

while (eof(i) = FALSE)
    line input #i, ln

    select case (trim(ln))
    case ";;;INSTALL;;;"
        emitInstallerFiles(manifest, o, TRUE)

    case ";;;UNINSTALL;;;"
        emitInstallerFiles(manifest, o, FALSE)

    case else
        ln = strReplace(ln, ";;;FBVERSION;;;", STRINGIFY(FBVERSION))
        print #o, ln

    end select
wend

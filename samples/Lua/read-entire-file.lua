--If the file opens with no problems, io.open will return a
--handle to the file with methods attached.
--If the file does not exist, io.open will return nil and
--an error message.
--assert will return the handle to the file if present, or
--it will throw an error with the message returned second
--by io.open.
local file = assert(io.open(filename))
--Without wrapping io.open in an assert, local file would be nil,
--which would cause an 'attempt to index a nil value' error when
--calling file:read.

--file:read takes the number of bytes to read, or a string for
--special cases, such as "*a" to read the entire file.
local contents = file:read'*a'

--If the file handle was local to the expression
--(ie. "assert(io.open(filename)):read'a'"),
--the file would remain open until its handle was
--garbage collected.
file:close()

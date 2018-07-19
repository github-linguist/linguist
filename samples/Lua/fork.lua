local posix = require 'posix'

local pid = posix.fork()
if pid == 0 then
    print("child process")
elseif pid > 0 then
    print("parent process")
else
    error("unable to fork")
end

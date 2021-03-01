include std/io.e

sequence cmds = command_line()
write_lines("command_line.txt", cmds[3..$])

def callStack = try { error("exception") } catch { case ex => ex.getStackTrace drop 2 }

def printStackTrace = callStack drop 1 /* don't print ourselves! */ foreach println

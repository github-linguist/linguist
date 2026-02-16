#include "git2_util.h"

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <io.h>
#include <direct.h>
#ifdef GIT_THREADS
 #include "win32/thread.h"
#endif

#include "git2.h"

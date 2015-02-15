#include <sys/types.h>
#include <dirent.h>
#include <regex.h>
#include <stdio.h>

enum {
    WALK_OK = 0,
    WALK_BADPATTERN,
    WALK_BADOPEN,
};

int walker(const char *dir, const char *pattern)
{
    struct dirent *entry;
    regex_t reg;
    DIR *d;

    if (regcomp(&reg, pattern, REG_EXTENDED | REG_NOSUB))
        return WALK_BADPATTERN;
    if (!(d = opendir(dir)))
        return WALK_BADOPEN;
    while (entry = readdir(d))
        if (!regexec(&reg, entry->d_name, 0, NULL, 0))
            puts(entry->d_name);
    closedir(d);
    regfree(&reg);
    return WALK_OK;
}

int main()
{
    walker(".", ".\\.c$");
    return 0;
}

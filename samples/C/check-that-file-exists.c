#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>

/* Check for regular file. */
int check_reg(const char *path) {
	struct stat sb;
	return stat(path, &sb) == 0 && S_ISREG(sb.st_mode);
}

/* Check for directory. */
int check_dir(const char *path) {
	struct stat sb;
	return stat(path, &sb) == 0 && S_ISDIR(sb.st_mode);
}

int main() {
	printf("input.txt is a regular file? %s\n",
	    check_reg("input.txt") ? "yes" : "no");
	printf("docs is a directory? %s\n",
	    check_dir("docs") ? "yes" : "no");
	printf("/input.txt is a regular file? %s\n",
	    check_reg("/input.txt") ? "yes" : "no");
	printf("/docs is a directory? %s\n",
	    check_dir("/docs") ? "yes" : "no");
	return 0;
}

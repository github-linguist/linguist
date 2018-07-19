#include <stdio.h>
#include <dirent.h>
#include <string.h>

int dir_empty(const char *path)
{
	struct dirent *ent;
	int ret = 1;

	DIR *d = opendir(path);
	if (!d) {
		fprintf(stderr, "%s: ", path);
		perror("");
		return -1;
	}

	while ((ent = readdir(d))) {
		if (!strcmp(ent->d_name, ".") || !(strcmp(ent->d_name, "..")))
			continue;
		ret = 0;
		break;
	}

	closedir(d);
	return ret;
}

int main(int c, char **v)
{
	int ret = 0, i;
	if (c < 2) return -1;

	for (i = 1; i < c; i++) {
		ret = dir_empty(v[i]);
		if (ret >= 0)
			printf("%s: %sempty\n", v[i], ret ? "" : "not ");
	}

	return 0;
}

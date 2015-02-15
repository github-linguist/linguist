#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void talk(const char *s)
{
	pid_t pid;
	int status;

	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(1);
	}

	if (pid == 0) {
		execlp("espeak", "espeak", s, (void*)0);
		perror("espeak");
		_exit(1);
	}

	waitpid(pid, &status, 0);
	if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
		exit(1);
}

int main()
{
	talk("This is an example of speech synthesis.");
	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <netinet/ip.h>

int tsocket;
struct sockaddr_in tsockinfo;
fd_set status, current;
void ClientText(int handle, char *buf, int buf_len);

struct client
{
    char buffer[4096];
    int pos;
    char name[32];
} *connections[FD_SETSIZE];

void AddConnection(int handle)
{
    connections[handle] = malloc(sizeof(struct client));
    connections[handle]->buffer[0] = '\0';
    connections[handle]->pos = 0;
    connections[handle]->name[0] = '\0';
}

void CloseConnection(int handle)
{
    char buf[512];
    int j;

    FD_CLR(handle, &status);

    if (connections[handle]->name[0])
    {
        sprintf(buf, "* Disconnected: %s\r\n", connections[handle]->name);

        for (j = 0; j < FD_SETSIZE; j++)
        {
            if (handle != j && j != tsocket && FD_ISSET(j, &status))
            {
                if (write(j, buf, strlen(buf)) < 0)
                {
                    CloseConnection(j);
                }
            }
        }
    } else
    {
        printf ("-- Connection %d disconnected\n", handle);
    }
    if (connections[handle])
    {
        free(connections[handle]);
    }
    close(handle);
}

void strip(char *buf)
{
    char *x;

    x = strchr(buf, '\n');
    if (x) { *x='\0'; }
    x = strchr(buf, '\r');
    if (x) { *x='\0'; }
}

int RelayText(int handle)
{
    char *begin, *end;
    int ret = 0;
    begin = connections[handle]->buffer;
    if (connections[handle]->pos == 4000)
    {
        if (begin[3999] != '\n')
            begin[4000] = '\0';
        else {
            begin[4000] = '\n';
            begin[4001] = '\0';
        }
    } else {
        begin[connections[handle]->pos] = '\0';
    }

    end = strchr(begin, '\n');
    while (end != NULL)
    {
        char output[8000];
        output[0] = '\0';
        if (!connections[handle]->name[0])
        {
            strncpy(connections[handle]->name, begin, 31);
            connections[handle]->name[31] = '\0';

            strip(connections[handle]->name);
            sprintf(output, "* Connected: %s\r\n", connections[handle]->name);
            ret = 1;
        } else
        {
            sprintf(output, "%s: %.*s\r\n", connections[handle]->name,
                    end-begin, begin);
            ret = 1;
        }

        if (output[0])
        {
            int j;
            for (j = 0; j < FD_SETSIZE; j++)
            {
                if (handle != j && j != tsocket && FD_ISSET(j, &status))
                {
                    if (write(j, output, strlen(output)) < 0)
                    {
                        CloseConnection(j);
                    }
                }
            }
        }
        begin = end+1;
        end = strchr(begin, '\n');
    }

    strcpy(connections[handle]->buffer, begin);
    connections[handle]->pos -= begin - connections[handle]->buffer;
    return ret;
}

void ClientText(int handle, char *buf, int buf_len)
{
    int i, j;
    if (!connections[handle])
        return;
    j = connections[handle]->pos;

    for (i = 0; i < buf_len; ++i, ++j)
    {
        connections[handle]->buffer[j] = buf[i];

        if (j == 4000)
        {
            while (RelayText(handle));
            j = connections[handle]->pos;
        }
    }
    connections[handle]->pos = j;

    while (RelayText(handle));
}


int ChatLoop()
{
    int i, j;
    FD_ZERO(&status);

    FD_SET(tsocket, &status);
    FD_SET(0, &status);

    while(1)
    {
        current = status;
        if (select(FD_SETSIZE, &current, NULL, NULL, NULL)==-1)
        {
            perror("Select");
            return 0;
        }
        for (i = 0; i < FD_SETSIZE; ++i)
        {
            if (FD_ISSET(i, &current))
            {
                if (i == tsocket)
                {
                    struct sockaddr_in cliinfo;
                    socklen_t addrlen = sizeof(cliinfo);
                    int handle;
                    handle = accept(tsocket, &cliinfo, &addrlen);
                    if (handle == -1)
                    {
                        perror ("Couldn't accept connection");
                    } else if (handle > FD_SETSIZE)
                    {
                        printf ("Unable to accept new connection.\n");
                        close(handle);
                    }
                    else
                    {
                        if (write(handle, "Enter name: ", 12) >= 0)
                        {
                            printf("-- New connection %d from %s:%hu\n",
                                handle,
                                inet_ntoa (cliinfo.sin_addr),
                                ntohs(cliinfo.sin_port));
                            FD_SET(handle, &status);

                            AddConnection(handle);
                        }
                    }
                } /* Read data, relay to others. */
                else
                {
                    char buf[512];
                    int b;

                    b = read(i, buf, 500);
                    if (b <= 0)
                    {
                        CloseConnection(i);
                    }
                    else
                    {
                        ClientText(i, buf, b);
                    }
                }
            }
        }
    } /* While 1 */
}

int main (int argc, char*argv[])
{
    tsocket = socket(PF_INET, SOCK_STREAM, 0);

    tsockinfo.sin_family = AF_INET;
    tsockinfo.sin_port = htons(7070);
    if (argc > 1)
    {
        tsockinfo.sin_port = htons(atoi(argv[1]));
    }
    tsockinfo.sin_addr.s_addr = htonl(INADDR_ANY);
    printf ("Socket %d on port %hu\n", tsocket, ntohs(tsockinfo.sin_port));

    if (bind(tsocket, &tsockinfo, sizeof(tsockinfo)) == -1)
    {
        perror("Couldn't bind socket");
        return -1;
    }

    if (listen(tsocket, 10) == -1)
    {
        perror("Couldn't listen to port");
    }

    ChatLoop();

    return 0;
}

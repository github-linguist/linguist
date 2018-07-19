#include <stdio.h>
#include <stdlib.h> /* malloc */
#include <string.h> /* strlen */
#define _XOPEN_SOURCE /* requred for time functions */
#define __USE_XOPEN
#include <time.h>
#define DB "database.csv" /* database name */
#define TRY(a)  if (!(a)) {perror(#a);exit(1);}
#define TRY2(a) if((a)<0) {perror(#a);exit(1);}
#define FREE(a) if(a) {free(a);a=NULL;}
#define sort_by(foo) \
static int by_##foo (const void*p1, const void*p2) { \
    return strcmp ((*(const pdb_t*)p1)->foo, (*(const pdb_t*)p2)->foo); }
typedef struct db {
    char title[26];
    char first_name[26];
    char last_name[26];
    time_t date;
    char publ[100];
    struct db *next;
}
db_t,*pdb_t;
typedef int (sort)(const void*, const void*);
enum {CREATE,PRINT,TITLE,DATE,AUTH,READLINE,READ,SORT,DESTROY};
static pdb_t dao (int cmd, FILE *f, pdb_t db, sort sortby);
static char *time2str (time_t *time);
static time_t str2time (char *date);
/* qsort callbacks */
sort_by(last_name);
sort_by(title);
static int by_date(pdb_t *p1, pdb_t *p2);
/* main */
int main (int argc, char **argv) {
    char buf[100];
    const char *commands[]={"-c", "-p", "-t", "-d", "-a", NULL};
    db_t db;
    db.next=NULL;
    pdb_t dblist;
    int i;
    FILE *f;
    TRY (f=fopen(DB,"a+"));
    if (argc<2) {
usage:  printf ("Usage: %s [commands]\n"
        "-c  Create new entry.\n"
        "-p  Print the latest entry.\n"
        "-t  Print all entries sorted by title.\n"
        "-d  Print all entries sorted by date.\n"
        "-a  Print all entries sorted by author.\n",argv[0]);
        fclose (f);
        return 0;
    }
    for (i=0;commands[i]&&strcmp(argv[1],commands[i]);i++);
    switch (i) {
        case CREATE:
        printf("-c  Create a new entry.\n");
        printf("Title           :");if((scanf(" %25[^\n]",db.title     ))<0)break;
        printf("Author Firstname:");if((scanf(" %25[^\n]",db.first_name))<0)break;
        printf("Author Lastname :");if((scanf(" %25[^\n]",db.last_name ))<0)break;
        printf("Date 10-12-2000 :");if((scanf(" %10[^\n]",buf          ))<0)break;
        printf("Publication     :");if((scanf(" %99[^\n]",db.publ      ))<0)break;
        db.date=str2time (buf);
        dao (CREATE,f,&db,NULL);
        break;
        case PRINT:
        printf ("-p  Print the latest entry.\n");
        while (!feof(f)) dao (READLINE,f,&db,NULL);
        dao (PRINT,f,&db,NULL);
        break;
        case TITLE:
        printf ("-t  Print all entries sorted by title.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,by_title);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        case DATE:
        printf ("-d  Print all entries sorted by date.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,(int (*)(const void *,const  void *)) by_date);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        case AUTH:
        printf ("-a  Print all entries sorted by author.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,by_last_name);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        default: {
            printf ("Unknown command: %s.\n",strlen(argv[1])<10?argv[1]:"");
            goto usage;
    }   }
    fclose (f);
    return 0;
}
/* Data Access Object (DAO) */
static pdb_t dao (int cmd, FILE *f, pdb_t in_db, sort sortby) {
    pdb_t *pdb=NULL,rec=NULL,hd=NULL;
    int i=0,ret;
    char buf[100];
    switch (cmd) {
        case CREATE:
        fprintf (f,"\"%s\",",in_db->title);
        fprintf (f,"\"%s\",",in_db->first_name);
        fprintf (f,"\"%s\",",in_db->last_name);
        fprintf (f,"\"%s\",",time2str(&in_db->date));
        fprintf (f,"\"%s\" \n",in_db->publ);
        break;
        case PRINT:
        for (;in_db;i++) {
            printf ("Title       : %s\n",     in_db->title);
            printf ("Author      : %s %s\n",  in_db->first_name, in_db->last_name);
            printf ("Date        : %s\n",     time2str(&in_db->date));
            printf ("Publication : %s\n\n",   in_db->publ);
            if (!((i+1)%3)) {
                printf ("Press Enter to continue.\n");
                ret = scanf ("%*[^\n]");
                if (ret<0) return rec; /* handle EOF */
                else getchar();
            }
            in_db=in_db->next;
        }
        break;
        case READLINE:
        if((fscanf(f," \"%[^\"]\",",in_db->title     ))<0)break;
        if((fscanf(f," \"%[^\"]\",",in_db->first_name))<0)break;
        if((fscanf(f," \"%[^\"]\",",in_db->last_name ))<0)break;
        if((fscanf(f," \"%[^\"]\",",buf              ))<0)break;
        if((fscanf(f," \"%[^\"]\" ",in_db->publ      ))<0)break;
        in_db->date=str2time (buf);
        break;
        case READ:
        while (!feof(f)) {
            dao (READLINE,f,in_db,NULL);
            TRY (rec=malloc(sizeof(db_t)));
            *rec=*in_db; /* copy contents */
            rec->next=hd;/* to linked list */
            hd=rec;i++;
        }
        if (i<2) {
            puts ("Empty database. Please create some entries.");
            fclose (f);
            exit (0);
        }
        break;
        case SORT:
        rec=in_db;
        for (;in_db;i++) in_db=in_db->next;
        TRY (pdb=malloc(i*sizeof(pdb_t)));
        in_db=rec;
        for (i=0;in_db;i++) {
            pdb[i]=in_db;
            in_db=in_db->next;
        }
        qsort (pdb,i,sizeof in_db,sortby);
        pdb[i-1]->next=NULL;
        for (i=i-1;i;i--) {
            pdb[i-1]->next=pdb[i];
        }
        rec=pdb[0];
        FREE (pdb);
        pdb=NULL;
        break;
        case DESTROY: {
            while ((rec=in_db)) {
                in_db=in_db->next;
                FREE (rec);
    }   }   }
    return rec;
}
/* convert numeric time to date string */
static char *time2str (time_t *time) {
    static char buf[255];
    struct tm *ptm;
    ptm=localtime (time);
    strftime(buf, 255, "%m-%d-%Y", ptm);
    return buf;
}
/* convert date string to numeric time */
static time_t str2time (char *date) {
    struct tm tm;
    memset (&tm, 0, sizeof(struct tm));
    strptime(date, "%m-%d-%Y", &tm);
    return mktime(&tm);
}
/* sort by date callback for qsort */
static int by_date (pdb_t *p1, pdb_t *p2) {
    if ((*p1)->date < (*p2)->date) {
        return -1;
    }
    else return ((*p1)->date > (*p2)->date);
}

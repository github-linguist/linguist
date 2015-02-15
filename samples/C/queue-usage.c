#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <sys/queue.h>

/* #include "fifolist.h" */

int main()
{
  int i;
  FIFOList head;

  TAILQ_INIT(&head);

  /* insert 20 integer values */
  for(i=0; i < 20; i++) {
    m_enqueue(i, &head);
  }

  /* dequeue and print */
  while( m_dequeue(&i, &head) )
    printf("%d\n", i);

  fprintf(stderr, "FIFO list %s\n",
      ( m_dequeue(&i, &head) ) ?
      "had still an element" :
      "is void!");

  exit(0);
}

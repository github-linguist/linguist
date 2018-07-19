#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

const char *names[] = {
  "April", "Tam O'Shanter", "Emily", NULL
};
const char *remarks[] = {
  "Bubbly: I'm > Tam and <= Emily",
  "Burns: \"When chapman billies leave the street ...\"",
  "Short & shrift", NULL
};

int main()
{
  xmlDoc *doc = NULL;
  xmlNode *root = NULL, *node;
  const char **next;
  int a;

  doc = xmlNewDoc("1.0");
  root = xmlNewNode(NULL, "CharacterRemarks");
  xmlDocSetRootElement(doc, root);

  for(next = names, a = 0; *next != NULL; next++, a++) {
    node = xmlNewNode(NULL, "Character");
    (void)xmlNewProp(node, "name", *next);
    xmlAddChild(node, xmlNewText(remarks[a]));
    xmlAddChild(root, node);
  }

  xmlElemDump(stdout, doc, root);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return EXIT_SUCCESS;
}

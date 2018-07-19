#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main()
{
  const char **next;
  int a;
  FILE *outFile;

  xmlDoc *doc = xmlNewDoc("1.0");
  xmlNode *root = xmlNewNode(NULL, "root");
  xmlDocSetRootElement(doc, root);

  xmlNode *node = xmlNewNode(NULL, "element");
  xmlAddChild(node, xmlNewText("some text here"));
  xmlAddChild(root, node);

  outFile = fopen("myfile.xml", "w");
  xmlElemDump(outFile, doc, root);
  fclose(outFile);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return EXIT_SUCCESS;
}

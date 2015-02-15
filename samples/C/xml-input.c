#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

static void print_names(xmlNode *node)
{
  xmlNode *cur_node = NULL;
  for (cur_node = node; cur_node; cur_node = cur_node->next) {
    if (cur_node->type == XML_ELEMENT_NODE) {
      if ( strcmp(cur_node->name, "Student") == 0 ) {
	xmlAttr *prop = NULL;
	if ( (prop = xmlHasProp(cur_node, "Name")) != NULL ) {
	  printf("%s\n", prop->children->content);
	
	}
      }
    }
    print_names(cur_node->children);
  }
}

const char *buffer =
  "<Students>\n"
  "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
  "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
  "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
  "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
  "    <Pet Type=\"dog\" Name=\"Rover\" />\n"
  "  </Student>\n"
  "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
  "</Students>\n";

int main()
{
  xmlDoc *doc = NULL;
  xmlNode *root = NULL;

  doc = xmlReadMemory(buffer, strlen(buffer), NULL, NULL, 0);
  if ( doc != NULL ) {
    root = xmlDocGetRootElement(doc);
    print_names(root);
    xmlFreeDoc(doc);
  }
  xmlCleanupParser();
  return 0;
}

#include <iostream>

void quote(char const* c)
{
  while (*c)
  {
    switch(*c)
    {
    case '\\':
      std::cout << "\\\\";break;
    case '\n':
      std::cout << "\\n";break;
    case '\"':
      std::cout << "\\\"";break;
    default:
      std::cout << *c;
    }
    ++c;
  }
}

int main()
{
  char const* parts[] = {
    "#include <iostream>\n\nvoid quote(char const* c)\n{\n  while (*c)\n  {\n    switch(*c)\n    {\n    case '\\\\':\n      std::cout << \"\\\\\\\\\";break;\n    case '\\n':\n      std::cout << \"\\\\n\";break;\n    case '\\\"':\n      std::cout << \"\\\\\\\"\";break;\n    default:\n      std::cout << *c;\n    }\n    ++c;\n  }\n}\n\nint main()\n{\n  char const* parts[] = {\n    \"",
    "\",\n    \"",
    "\"\n  };\n  \n  std::cout << parts[0];\n  quote(parts[0]);\n  std::cout << parts[1];\n  quote(parts[1]);\n  std::cout << parts[1];\n  quote(parts[2]);\n  std::cout << parts[2];\n\n  return 0;\n}\n"
  };

  std::cout << parts[0];
  quote(parts[0]);
  std::cout << parts[1];
  quote(parts[1]);
  std::cout << parts[1];
  quote(parts[2]);
  std::cout << parts[2];

  return 0;
}

string in = "<Students>\n"
            "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
            "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
            "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
            "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
            "    <Pet Type=\"dog\" Name=\"Rover\" />\n"
            "  </Student>\n"
            "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
            "</Students>\n";

object s = Parser.XML.Tree.simple_parse_input(in);

array collect = ({});
s->walk_inorder(lambda(object node)
                {
                    if (node->get_tag_name() == "Student")
                        collect += ({ node->get_attributes()->Name });
                });
write("%{%s\n%}", collect);

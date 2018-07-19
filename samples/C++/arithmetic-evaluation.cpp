 #include <boost/spirit.hpp>
 #include <boost/spirit/tree/ast.hpp>
 #include <string>
 #include <cassert>
 #include <iostream>
 #include <istream>
 #include <ostream>

 using boost::spirit::rule;
 using boost::spirit::parser_tag;
 using boost::spirit::ch_p;
 using boost::spirit::real_p;

 using boost::spirit::tree_node;
 using boost::spirit::node_val_data;

 // The grammar
 struct parser: public boost::spirit::grammar<parser>
 {
   enum rule_ids { addsub_id, multdiv_id, value_id, real_id };

   struct set_value
   {
     set_value(parser const& p): self(p) {}
     void operator()(tree_node<node_val_data<std::string::iterator,
                                             double> >& node,
                     std::string::iterator begin,
                     std::string::iterator end) const
     {
       node.value.value(self.tmp);
     }
     parser const& self;
   };

   mutable double tmp;

   template<typename Scanner> struct definition
   {
     rule<Scanner, parser_tag<addsub_id> > addsub;
     rule<Scanner, parser_tag<multdiv_id> > multdiv;
     rule<Scanner, parser_tag<value_id> > value;
     rule<Scanner, parser_tag<real_id> > real;

     definition(parser const& self)
     {
       using namespace boost::spirit;
       addsub = multdiv
         >> *((root_node_d[ch_p('+')] | root_node_d[ch_p('-')]) >> multdiv);
       multdiv = value
         >> *((root_node_d[ch_p('*')] | root_node_d[ch_p('/')]) >> value);
       value = real | inner_node_d[('(' >> addsub >> ')')];
       real = leaf_node_d[access_node_d[real_p[assign_a(self.tmp)]][set_value(self)]];
     }

     rule<Scanner, parser_tag<addsub_id> > const& start() const
     {
       return addsub;
     }
   };
 };

 template<typename TreeIter>
 double evaluate(TreeIter const& i)
 {
   double op1, op2;
   switch (i->value.id().to_long())
   {
   case parser::real_id:
     return i->value.value();
   case parser::value_id:
   case parser::addsub_id:
   case parser::multdiv_id:
     op1 = evaluate(i->children.begin());
     op2 = evaluate(i->children.begin()+1);
     switch(*i->value.begin())
     {
     case '+':
       return op1 + op2;
     case '-':
       return op1 - op2;
     case '*':
       return op1 * op2;
     case '/':
       return op1 / op2;
     default:
       assert(!"Should not happen");
     }
   default:
     assert(!"Should not happen");
   }
   return 0;
 }

 // the read/eval/write loop
 int main()
 {
   parser eval;
   std::string line;
   while (std::cout << "Expression: "
          && std::getline(std::cin, line)
          && !line.empty())
   {
     typedef boost::spirit::node_val_data_factory<double> factory_t;
     boost::spirit::tree_parse_info<std::string::iterator, factory_t> info =
       boost::spirit::ast_parse<factory_t>(line.begin(), line.end(),
                                           eval, boost::spirit::space_p);
     if (info.full)
     {
       std::cout << "Result: " << evaluate(info.trees.begin()) << std::endl;
     }
     else
     {
       std::cout << "Error in expression." << std::endl;
     }
   }
 };

//  Create an HTML Table from comma seperated values
//  Nigel Galloway - June 2nd., 2013
grammar csv2html;
dialog : {System.out.println("<HTML><Table>");}header body+{System.out.println("</Table></HTML>");} ;
header : {System.out.println("<THEAD align=\"center\"><TR bgcolor=\"blue\">");}row{System.out.println("</TR></THEAD");};
body   : {System.out.println("<TBODY><TR>");}row{System.out.println("</TR></TBODY");};
row    : field ',' field '\r'? '\n';
field  : Field{System.out.println("<TD>" + $Field.text.replace("<","&lt;").replace(">","&gt;") + "</TD>");};
Field  : ~[,\n\r]+;

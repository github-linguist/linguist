USING: classes.singleton kernel io prettyprint ;
IN: singleton-demo

SINGLETON: bar
GENERIC: foo ( obj -- )
M: bar foo drop "Hello!" print ;

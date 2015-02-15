Column[Cases[Import["test.xml","XML"],Rule["Name", n_ ] -> n,Infinity]]

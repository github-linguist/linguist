for Dir in ["/" "./"] do
   File = {New Open.file init(name:Dir#"output.txt" flags:[create])}
in
   {File close}
   {OS.mkDir Dir#"docs" ['S_IRUSR' 'S_IWUSR' 'S_IXUSR' 'S_IXGRP']}
end

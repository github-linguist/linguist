for Dir in ["/" "./"] do
   try {OS.unlink Dir#"output.txt"}
   catch _ then {System.showInfo "File does not exist."} end
   try {OS.rmDir Dir#"docs"}
   catch _ then {System.showInfo "Directory does not exist."} end
end

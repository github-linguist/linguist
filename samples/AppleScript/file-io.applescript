on copyFile from src into dst
       set filedata to read file src
       set outfile to open for access dst with write permission
       write filedata to outfile
       close access outfile
end copyFile

copyFile from ":input.txt" into ":output.txt"

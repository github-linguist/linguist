var file, str;
file = file_text_open_read("input.txt");
str = "";
while (true)
    {
    str += file_text_read_string(file);
    if (file_text_eof(file))
        {
        break;
        }
    else
        {
        str += chr(vk_enter);
        file_text_readln(file);
        }
    }
file_text_close(file);

file = file_text_open_write("output.txt");
file_text_write_string(file,str);
file_text_close(file);

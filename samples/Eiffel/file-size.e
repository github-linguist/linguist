class
    APPLICATION
create
    make
feature {NONE} -- Initialization
    make
            -- Run application.
        do
            create input_file.make_open_read ("input.txt")
            print(input_file.count)
            print("%N")
            input_file.close
            create environment
            input_file.make_open_read(environment.root_directory_name + "input.txt")
            print(input_file.count)
            input_file.close
        end
feature -- Access
    input_file: PLAIN_TEXT_FILE
    environment:EXECUTION_ENVIRONMENT
end

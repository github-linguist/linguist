class
    APPLICATION

create
    make

feature {NONE} -- Initialization

    make
            -- Run application.
        do
            create input_file.make_open_read ("input.txt")
            create output_file.make_open_write ("output.txt")

            from
                input_file.read_character
            until
                input_file.exhausted
            loop
                output_file.put (input_file.last_character)
                input_file.read_character
            end

            input_file.close
            output_file.close
        end

feature -- Access

    input_file: PLAIN_TEXT_FILE
    output_file: PLAIN_TEXT_FILE

end

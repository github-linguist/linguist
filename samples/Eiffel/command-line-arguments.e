class
    APPLICATION
inherit
    ARGUMENTS
create
    make
feature {NONE} -- Initialization
    make
            -- Print values for arguments with options 'c' and 'h'.
        do
            print ("Command line argument value for option 'c' is: ")
            print (separate_character_option_value ('c') + "%N")
            print ("Command line argument value for option 'h' is: ")
            print (separate_character_option_value ('h') + "%N")
            io.read_line    -- Keep console window open
        end
end

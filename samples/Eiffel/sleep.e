class
    APPLICATION
inherit
    EXECUTION_ENVIRONMENT
create
    make
feature -- Initialization
    make
            -- Sleep for a given number of nanoseconds.
        do
            print ("Enter a number of nanoseconds: ")
            io.read_integer_64
            print ("Sleeping...%N")
            sleep (io.last_integer_64)
            print ("Awake!%N")
        end
end

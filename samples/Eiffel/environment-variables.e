class
    APPLICATION
inherit
    EXECUTION_ENVIRONMENT
create
    make
feature {NONE} -- Initialization
    make
            -- Retrieve and print value for environment variable `USERNAME'.
        do
            print (get ("USERNAME"))
        end
end

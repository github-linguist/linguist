class
    APPLICATION
create
    make
feature
    make
            -- Demonstrate string reversal.
        do
            my_string := "Hello World!"
            my_string.mirror
            print (my_string)
        end
    my_string: STRING
            -- Used for reversal
end

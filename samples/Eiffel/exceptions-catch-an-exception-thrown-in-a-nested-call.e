class MAIN
    inherit EXCEPTIONS

    creation foo

feature {ANY}
    baz_calls: INTEGER

    feature foo is
        do
            Current.bar
        rescue
            if is_developer_exception_of_name("U0") then
                baz_calls := 1
                print("Caught U0 exception.%N")
                retry
            end
            if is_developer_exception then
                print("Won't catch ")
                print(developer_exception_name)
                print(" exception...%N")
            end
        end

    feature bar is
        do
            Current.baz
        end

    feature baz is
        do
            if baz_calls = 0 then
                raise("U0")
            else
                raise("U1")
            end
        end
end

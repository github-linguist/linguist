class
    MY_SORTED_SET [G -> COMPARABLE]
inherit
    TWO_WAY_SORTED_SET [G]
        redefine
            sort
        end
create
    make

feature

    sort
            -- Shell sort
        local
            inc: INTEGER
            j: INTEGER
            l_value: like item
        do
            from
                inc := (count.to_double / 2.0).rounded
            until
                inc <= 0
            loop
                across inc |..| (count - 1) as ii
                loop
                    l_value := Current [ii.item + 1]
                    from
                        j := ii.item
                    until
                        j < inc or Current [j - inc + 1] <= l_value
                    loop
                        Current [j + 1] := Current [j - inc + 1]
                        j := j - inc
                    end
                    Current [j + 1] := l_value
                end
                inc := (inc.to_double / 2.2).rounded
            end
        end

end

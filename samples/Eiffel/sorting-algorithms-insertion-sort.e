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
            -- Insertion sort
        local
            l_j: INTEGER
            l_value: like item
        do
            across 2 |..| count as ii loop
                from
                    l_j := ii.item - 1
                    l_value := Current.i_th (ii.item)
                until
                    l_j < 1 or Current.i_th (l_j) <= l_value
                loop
                    Current.i_th (l_j + 1) := Current.i_th (l_j)
                    l_j := l_j - 1
                end
                Current.i_th (l_j + 1) := l_value
            end
        end

end

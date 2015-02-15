import java.util.ArrayList
import java.util.List
import java.util.Collection

def flatten(list: Collection)
    flatten(list, ArrayList.new)
end
def flatten(source: Collection, result: List)

    source.each do |x|
        if x.kind_of?(Collection)
            flatten(Collection(x), result)
        else
            result.add(x)
            result  # if branches must return same type
        end
    end
    result
end

# creating a list-of-list-of-list fails currently, so constructor calls are needed
source = [[1], 2, [[3, 4], 5], [[ArrayList.new]], [[[6]]], 7, 8, ArrayList.new]

puts flatten(source)

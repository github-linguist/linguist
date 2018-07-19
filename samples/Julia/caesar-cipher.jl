function rot(s::String, key::Integer)
    map(s) do c
        if 'a' <= c <= 'z'
            char( mod(c - 'a' + key, 26) + 'a')
        elseif 'A' <= c <= 'Z'
            char( mod(c - 'A' + key, 26) + 'A')
        else
            c
        end
    end
end

key = 3
txt = "The five boxing wizards jump quickly"

println("Original:  ", txt);
println("Encrypted: ", rot(txt, key))
println("Decrypted: ", rot(rot(txt, key), 26 - key))

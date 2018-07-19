function Encrypt( _msg, _key )
    local msg = { _msg:upper():byte( 1, -1 ) }
    local key = { _key:upper():byte( 1, -1 ) }
    local enc = {}

    local j, k = 1, 1
    for i = 1, #msg do
        if msg[i] >= string.byte('A') and msg[i] <= string.byte('Z') then
            enc[k] = ( msg[i] + key[j] - 2*string.byte('A') ) % 26 + string.byte('A')

            k = k + 1
            if j == #key then j = 1 else j = j + 1 end
        end
    end

    return string.char( unpack(enc) )
end

function Decrypt( _msg, _key )
    local msg = { _msg:byte( 1, -1 ) }
    local key = { _key:upper():byte( 1, -1 ) }
    local dec = {}

    local j = 1
    for i = 1, #msg do
       dec[i] = ( msg[i] - key[j] + 26 ) % 26 + string.byte('A')

       if j == #key then j = 1 else j = j + 1 end
    end

    return string.char( unpack(dec) )
end


original = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
key = "VIGENERECIPHER";

encrypted = Encrypt( original, key )
decrypted = Decrypt( encrypted, key )

print( encrypted )
print( decrypted )

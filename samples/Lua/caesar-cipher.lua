local function encrypt(text, key)
	return text:gsub("%a", function(t)
			local base = (t:lower() == t and string.byte('a') or string.byte('A'))

			local r = t:byte() - base
			r = r + key
			r = r%26 -- works correctly even if r is negative
			r = r + base
			return string.char(r)
		end)
end

local function decrypt(text, key)
	return encrypt(text, -key)
end

caesar = {
	encrypt = encrypt,
	decrypt = decrypt,
}

-- test
do
	local text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz"
	local encrypted = caesar.encrypt(text, 7)
	local decrypted = caesar.decrypt(encrypted, 7)
	print("Original text:  ", text)
	print("Encrypted text: ", encrypted)
	print("Decrypted text: ", decrypted)
end

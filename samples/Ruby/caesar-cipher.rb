module CaesarCipher
  AtoZ = (0..25).each_with_object({}) do |key,h|
    str = [*"A".."Z"].rotate(key).join
    h[key] = str + str.downcase
  end

  def encrypt(key, plaintext)
    (1..25) === key or raise ArgumentError, "key not in 1..25"
    plaintext.tr(AtoZ[0], AtoZ[key])
  end

  def decrypt(key, ciphertext)
    (1..25) === key or raise ArgumentError, "key not in 1..25"
    ciphertext.tr(AtoZ[key], AtoZ[0])
  end
end

include CaesarCipher

original = "THEYBROKEOURCIPHEREVERYONECANREADTHIS"
en = encrypt(3, original)
de = decrypt(3, en)

[original, en, de].each {|e| puts e}

puts 'OK' if
  (1..25).all? {|k| original == decrypt(k, encrypt(k, original))}

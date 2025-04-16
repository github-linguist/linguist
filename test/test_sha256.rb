require_relative "./helper"

class TestSHA256 < Minitest::Test
  include Linguist

  def test_hexdigest_string
    assert_equal "15020d93a6f635366cb20229cb3931c3651992dc6df85cddecc743fa11e48a66", SHA256.hexdigest("foo")
    assert_equal "3da77c2b08c7d29fe3d936b7918039941c0881065dde07d0af9d280d2d475d00", SHA256.hexdigest("bar")
  end

  def test_hexdigest_symbol
    assert_equal "dea6712e86478d2ee22a35a8c5ac9627e7cbc5ce2407a7da7c645fea2434fe9b", SHA256.hexdigest(:foo)
    assert_equal "9e414b095a78ef4c2ae6f74dc6898f653c6590554ff36719d4803733a6d910e3", SHA256.hexdigest(:bar)

    refute_equal SHA256.hexdigest("foo"), SHA256.hexdigest(:foo)
  end

  def test_hexdigest_integer
    # Ruby 2.4.0 merged Bignum and Fixnum into Integer which means we get different digests
    if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("2.4.0")
      assert_equal "e2e98cd680460c0cecd76714d94e601be9b629ad16ae45251fa30366f62007bb", SHA256.hexdigest(1)
      assert_equal "88159e413c4a97194ee0d8b082ebfd54ce3f9985b58270d3bf59a6e6186f8da6", SHA256.hexdigest(2)
    else
      assert_equal "d9af1c504ce3dc080973d6de94d44884482fcee3ff04fd430c5aba2e4d1ba2cd", SHA256.hexdigest(1)
      assert_equal "1a4dfbfcf67bc80f3dbade7083386362d8f40db225cbd8e41e61199599f334c3", SHA256.hexdigest(2)
    end

    refute_equal SHA256.hexdigest("1"), SHA256.hexdigest(1)
  end

  def test_hexdigest_boolean
    assert_equal "92de503a8b413365fc38050c7dd4bacf28b0f705e744dacebcaa89f2032dcd67", SHA256.hexdigest(true)
    assert_equal "bdfd64a7c8febcc3b0b8fb05d60c8e2a4cb6b8c081fcba20db1c9778e9beaf89", SHA256.hexdigest(false)

    refute_equal SHA256.hexdigest("true"), SHA256.hexdigest(true)
    refute_equal SHA256.hexdigest("false"), SHA256.hexdigest(false)
  end

  def test_hexdigest_nil
    assert_equal "9bda381dac87b1c16b04f996abb623f43f1cdb89ce8be7dda3f67319dc440bc5", SHA256.hexdigest(nil)

    refute_equal SHA256.hexdigest("nil"), SHA256.hexdigest(nil)
  end

  def test_hexdigest_array
    assert_equal "f0cf39d0be3efbb6f86ac2404100ff7e055c17ded946a06808d66f89ca03a811", SHA256.hexdigest([])
    # Ruby 2.4.0 merged Bignum and Fixnum into Integer which means we get different digests
    if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("2.4.0")
      assert_equal "e0ca0f6a274c6cc8e07fce6cf58cd2feef978928cad1c0760b6c3b8a865eafaf", SHA256.hexdigest([1])
      assert_equal "46b40ee5dcee74020fd75532659329e2a01b8cd74727218966c40cd1b4a54dab", SHA256.hexdigest([1, 2])
      assert_equal "0c950e57182b6830a3f4da3388fa7e3e5ec879277847c20b941f5a0144ba53a7", SHA256.hexdigest([1, 2, 3])
      assert_equal "0ef88eec0c680d3860c0001c6beb4407559d61db85744e088436a21d72425b4f", SHA256.hexdigest([1, 2, [3]])
    else
      assert_equal "87d177e40c167e5ce164081297c788c36540aa8f9cc05a0d1d3abfa577b56886", SHA256.hexdigest([1])
      assert_equal "c8189462e4785c786b96c8d29ecfa7c030010700dd1dfb194b84b37c50981feb", SHA256.hexdigest([1, 2])
      assert_equal "7edd426189105eede4954333ce40d96ff850ca86aa48e15b58cd017aab52c712", SHA256.hexdigest([1, 2, 3])
      assert_equal "b0f08d52b5efd5c44dc47965733ff8baac34db05abf2620a1f71c0fc0a7e42d2", SHA256.hexdigest([1, 2, [3]])
    end
  end

  def test_hexdigest_hash
    assert_equal "a91069147f9bd9245cdacaef8ead4c3578ed44f179d7eb6bd4690e62ba4658f2", SHA256.hexdigest({})
    # Ruby 2.4.0 merged Bignum and Fixnum into Integer which means we get different digests
    if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("2.4.0")
      assert_equal "124c84e8bf1fe4be4d2c3e5d79522972f390dbf6031dd8ba7029793f2fec0ef7", SHA256.hexdigest({:a => 1})
      assert_equal "8a1c987468032f979d4a7d4eeb5049bfe0aa04acea54a4456888e2898dd1c532", SHA256.hexdigest({:b => 2})
    else
      assert_equal "7ae6ddefc4ec3fca1ca84794904144cdc99ca24444fcb44c05c0f269af8c8840", SHA256.hexdigest({:a => 1})
      assert_equal "484ca3e0efd43e7d650189150751512a3c985957f4c15b52da0bb12d72b3dcaa", SHA256.hexdigest({:b => 2})
    end

    refute_equal SHA256.hexdigest([:b, 2]), SHA256.hexdigest({:b => 2})

    assert_equal SHA256.hexdigest({:b => 2, :a => 1}), SHA256.hexdigest({:a => 1, :b => 2})
    assert_equal SHA256.hexdigest({:c => 3, :b => 2, :a => 1}), SHA256.hexdigest({:a => 1, :b => 2, :c => 3})
  end
end

irb(main):001:0> require 'tempfile'
=> true
irb(main):002:0> f = Tempfile.new('foo')
=> #<File:/tmp/foo20081226-307-10p746n-0>
irb(main):003:0> f.path
=> "/tmp/foo20081226-307-10p746n-0"
irb(main):004:0> f.close
=> nil

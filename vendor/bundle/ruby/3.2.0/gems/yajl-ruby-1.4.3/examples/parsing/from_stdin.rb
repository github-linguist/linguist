$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl'

# Usage: cat benchmark/subjects/item.json | ruby examples/from_stdin.rb

hash = Yajl::Parser.parse(STDIN)
puts hash.inspect

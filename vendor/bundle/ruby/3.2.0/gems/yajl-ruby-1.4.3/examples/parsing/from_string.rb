$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl'
require 'stringio'

unless string = ARGV[0]
  puts "\nUsage: ruby examples/from_string.rb '{\"foo\": 1145}'\n\n"
  exit(0)
end

hash = Yajl::Parser.parse(string)
puts hash.inspect

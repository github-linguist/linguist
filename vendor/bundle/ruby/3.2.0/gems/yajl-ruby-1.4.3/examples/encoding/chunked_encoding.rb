$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl'

obj = {
  :a_test => "of encoding in one pass" * 512,
  :a_test2 => "of encoding in one pass" * 512,
  :a_test3 => "of encoding in one pass" * 512,
  :a_test4 => "of encoding in one pass" * 512,
  :which_will => "simply return a string when finished" * 512,
  :which_will2 => "simply return a string when finished" * 512,
  :which_will3 => "simply return a string when finished" * 512,
  :which_will4 => "simply return a string when finished" * 512,
  :as_easy_as => 123
}

chunks = 0
total_size = 0

Yajl::Encoder.encode(obj) do |chunk|
  chunks += 1
  total_size += chunk.size
  STDOUT << chunk
end

puts "\n\nEncoder generated #{total_size} bytes of data, in #{chunks} chunks"

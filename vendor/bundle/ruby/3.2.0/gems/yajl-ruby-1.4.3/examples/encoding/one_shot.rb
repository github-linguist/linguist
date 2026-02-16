$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl'

obj = {
  :a_test => "of encoding in one pass",
  :which_will => "simply return a string when finished",
  :as_easy_as => 123
}

str = Yajl::Encoder.encode(obj)
puts str

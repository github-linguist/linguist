$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl/http_stream'
require 'uri'

unless keywords = ARGV[0]
  puts "\nUsage: ruby examples/http/twitter_search_api.rb keyword\n\n"
  exit(0)
end

puts Yajl::HttpStream.get("http://search.twitter.com/search.json?q=#{keywords}").inspect

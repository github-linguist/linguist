$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../..')
$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../../lib')

require 'yajl/gzip'
require 'yajl/deflate'
require 'yajl/http_stream'
require 'uri'

unless (username = ARGV[0]) && (password = ARGV[1])
  puts "\nUsage: ruby examples/http/twitter_stream_api.rb username password\n\n"
  exit(0)
end
captured = 0
uri = URI.parse("http://#{username}:#{password}@stream.twitter.com/1/statuses/sample.json")

trap('INT') {
  puts "\n\nCaptured #{captured} objects from the stream"
  puts "CTRL+C caught, later!"
  exit(0)
}

Yajl::HttpStream.get(uri, :symbolize_keys => true) do |hash|
  STDOUT.putc '.'
  STDOUT.flush
  captured += 1
end

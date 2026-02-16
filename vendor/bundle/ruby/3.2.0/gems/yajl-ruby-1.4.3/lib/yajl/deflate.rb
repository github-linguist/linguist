puts "DEPRECATION WARNING: Yajl's Deflate support is going to be removed in 2.0"

require 'yajl' unless defined?(Yajl::Parser)
require 'zlib' unless defined?(Zlib)
require 'yajl/deflate/stream_reader.rb'
require 'yajl/deflate/stream_writer.rb'
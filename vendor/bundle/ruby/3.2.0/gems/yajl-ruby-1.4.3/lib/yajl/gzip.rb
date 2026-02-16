puts "DEPRECATION WARNING: Yajl's Gzip support is going to be removed in 2.0"

require 'yajl' unless defined?(Yajl::Parser)
require 'zlib' unless defined?(Zlib)
require 'yajl/gzip/stream_reader.rb'
require 'yajl/gzip/stream_writer.rb'
puts "DEPRECATION WARNING: Yajl's Bzip2 support is going to be removed in 2.0"

require 'yajl' unless defined?(Yajl::Parser)

begin
  require 'bzip2' unless defined?(Bzip2)
  require 'yajl/bzip2/stream_reader.rb'
  require 'yajl/bzip2/stream_writer.rb'
rescue LoadError
  raise "Unable to load the bzip2 library. Is the bzip2-ruby gem installed?"
end

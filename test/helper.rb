require "bundler/setup"
require "minitest/autorun"
require "mocha/setup"
require "linguist"
require 'color-proximity'
require "linguist/blob"
require 'licensee'

def fixtures_path
  File.expand_path("../fixtures", __FILE__)
end

def fixture_blob(name)
  filepath = (name =~ /^\//)? name : File.join(fixtures_path, name)
  Linguist::FileBlob.new(filepath, fixtures_path)
end

def fixture_blob_memory(name)
  filepath = (name =~ /^\//)? name : File.join(fixtures_path, name)
  content = File.read(filepath)
  Linguist::Blob.new(name, content)
end

def samples_path
  File.expand_path("../../samples", __FILE__)
end

def sample_blob(name)
  filepath = (name =~ /^\//)? name : File.join(samples_path, name)
  Linguist::FileBlob.new(filepath, samples_path)
end

def sample_blob_memory(name)
  filepath = (name =~ /^\//)? name : File.join(samples_path, name)
  content = File.read(filepath)
  Linguist::Blob.new(name, content)
end

def silence_warnings
  original_verbosity = $VERBOSE
  $VERBOSE = nil
  yield
ensure
  $VERBOSE = original_verbosity
end

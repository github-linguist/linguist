require "bundler/setup"
require "minitest/autorun"
require "mocha/setup"
require "linguist"
require 'color-proximity'
require 'licensee'

def fixtures_path
  File.expand_path("../fixtures", __FILE__)
end

def fixture_blob(name)
  name = File.join(fixtures_path, name) unless name =~ /^\//
  Linguist::FileBlob.new(name, fixtures_path)
end

def samples_path
  File.expand_path("../../samples", __FILE__)
end

def sample_blob(name)
  name = File.join(samples_path, name) unless name =~ /^\//
  Linguist::FileBlob.new(name, samples_path)
end

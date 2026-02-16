#Copyright (c) 2012 Vasiliy Ermolovich
#Copyright (c) 2014 Justin W Smith

require 'spec_helper'
require 'stringio'
require 'yaml'

describe XXhash do
  hash32 = YAML.load(IO.read "spec/results32.yaml")
  hash64 = YAML.load(IO.read "spec/results64.yaml")

  hash32.each do |key, value|
    it 'returns correct hash' do
      expect(XXhash.xxh32(key[0], key[1])).to eq(value)
    end
  end

  hash64.each do |key, value|
    it 'returns correct hash' do
      expect(XXhash.xxh64(key[0], key[1])).to eq(value)
    end
  end

  describe 'StreamingHash' do

    hash32.each do |key, value|
      it 'returns correct hash' do
        expect(XXhash.xxh32_stream(StringIO.new(key[0]), key[1])).to eq(value)
      end
    end

    it 'returns same hash for streamed files' do
      h1 = XXhash.xxh32(File.read(__FILE__), 123)
      h2 = XXhash.xxh32_stream(File.open(__FILE__), 123)
      expect(h1).to eq(h2)
    end

    hash64.each do |key, value|
      it 'returns correct hash' do
        expect(XXhash.xxh64_stream(StringIO.new(key[0]), key[1])).to eq(value)
      end
    end

    it 'returns same hash for streamed files' do
      h1 = XXhash.xxh64(File.read(__FILE__), 123)
      h2 = XXhash.xxh64_stream(File.open(__FILE__), 123)
      expect(h1).to eq(h2)
    end
  end

  def use_external_hash hash, io, chunk_size=1024
    while chunk=io.read(chunk_size)
      hash.update(chunk)
    end
    hash.digest
  end

  describe 'Digest::XXHash32' do

    it 'returns the hash for streamed strings' do
      StringIO.open('test') do |io|
        xxhash = Digest::XXHash32.new(123)
        result = use_external_hash xxhash, io
        expect(result).to eq(2758658570)
      end
    end

    it 'returns the hash for streamed files' do
      h1 = XXhash.xxh32(File.read(__FILE__), 123)
      xxhash = Digest::XXHash32.new(123)
      result = use_external_hash xxhash, File.open(__FILE__)
      expect(result).to eq(h1)
    end

    it 'returns correct hash after a reset' do
      h1 = XXhash.xxh32(File.read(__FILE__), 123)
      xxhash = Digest::XXHash32.new(123)
      expect(xxhash.digest('test')).to eq(2758658570)
      xxhash.reset
      result = use_external_hash xxhash, File.open(__FILE__)
      expect(result).to eq(h1)
    end
  end

  describe 'Digest::XXHash64' do

    it 'returns the hash for streamed strings' do
      StringIO.open('test') do |io|
        xxhash = Digest::XXHash64.new(123)
        result = use_external_hash xxhash, io
        expect(result).to eq(3134990500624303823)
      end
    end

    it 'returns the hash for streamed files' do
      h1 = XXhash.xxh64(File.read(__FILE__), 123)
      xxhash = Digest::XXHash64.new(123)
      result = use_external_hash xxhash, File.open(__FILE__)
      expect(result).to eq(h1)
    end

    it 'returns correct hash after a reset' do
      h1 = XXhash.xxh64(File.read(__FILE__), 123)
      xxhash = Digest::XXHash64.new(123)
      expect(xxhash.digest('test')).to eq(3134990500624303823)
      xxhash.reset
      result = use_external_hash xxhash, File.open(__FILE__)
      expect(result).to eq(h1)
    end
  end

end

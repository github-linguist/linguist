require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

describe "Yajl" do
  context "dump" do
    it "should exist as a class-method" do
      expect(Yajl).to respond_to(:dump)
    end

    it "should be able to encode to a string" do
      expect(Yajl.dump({:a => 1234})).to eql('{"a":1234}')
    end

    it "should be able to encode to an IO" do
      io = StringIO.new
      Yajl.dump({:a => 1234}, io)
      io.rewind
      expect(io.read).to eql('{"a":1234}')
    end

    it "should be able to encode with a block supplied" do
      Yajl.dump({:a => 1234}) do |chunk|
        expect(chunk).to eql('{"a":1234}')
      end
    end
  end

  context "load" do
    it "should exist as a class-method" do
      expect(Yajl).to respond_to(:load)
    end

    it "should be able to parse from a string" do
      expect(Yajl.load('{"a":1234}')).to eql({"a" => 1234})
    end

    it "should be able to parse from an IO" do
      io = StringIO.new('{"a":1234}')
      expect(Yajl.load(io)).to eql({"a" => 1234})
    end

    it "should be able to parse from a string with a block supplied" do
      Yajl.load('{"a":1234}') do |h|
        expect(h).to eql({"a" => 1234})
      end
    end

    it "should be able to parse from an IO with a block supplied" do
      io = StringIO.new('{"a":1234}')
      Yajl.load(io) do |h|
        expect(h).to eql({"a" => 1234})
      end
    end
  end
end

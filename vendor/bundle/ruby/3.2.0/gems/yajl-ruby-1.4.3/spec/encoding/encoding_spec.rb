# encoding: UTF-8
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')
require 'tmpdir'
require 'zlib'

class Dummy2
  def to_json
    "{\"hawtness\":true}"
  end
end

class TheMindKiller
  def to_json
    nil
  end
end

class TheMindKillerDuce
  def to_s
    nil
  end
end

describe "Yajl JSON encoder" do
  FILES = Dir[File.dirname(__FILE__)+'/../../benchmark/subjects/*.json']

  FILES.each do |file|
     it "should encode #{File.basename(file)} to an StringIO" do
       # we don't care about testing the stream subject as it has multiple JSON strings in it
       if File.basename(file) != 'twitter_stream.json'
         input = File.new(File.expand_path(file), 'r')
         io = StringIO.new
         encoder = Yajl::Encoder.new
         hash = Yajl::Parser.parse(input)
         encoder.encode(hash, io)
         io.rewind
         hash2 = Yajl::Parser.parse(io)
         io.close
         input.close
         expect(hash).to eq(hash2)
       end
     end
   end

   FILES.each do |file|
     it "should encode #{File.basename(file)} to a Zlib::GzipWriter" do
      # we don't care about testing the stream subject as it has multiple JSON strings in it
      if File.basename(file) != 'twitter_stream.json'
         hash = File.open(File.expand_path(file), 'r') do |input|
            Yajl::Parser.parse(input)
         end
         hash2 = Dir.mktmpdir do |tmp_dir|
            output_filename = File.join(tmp_dir, 'output.json')
            Zlib::GzipWriter.open(output_filename) do |writer|
               Yajl::Encoder.encode(hash, writer)
            end
            Zlib::GzipReader.open(output_filename) do |reader|
               Yajl::Parser.parse(reader.read)
            end
         end
         expect(hash).to eq(hash2)
      end
     end
   end

   FILES.each do |file|
     it "should encode #{File.basename(file)} and return a String" do
       # we don't care about testing the stream subject as it has multiple JSON strings in it
       if File.basename(file) != 'twitter_stream.json'
         input = File.new(File.expand_path(file), 'r')
         encoder = Yajl::Encoder.new
         hash = Yajl::Parser.parse(input)
         output = encoder.encode(hash)
         hash2 = Yajl::Parser.parse(output)
         input.close
         expect(hash).to eq(hash2)
       end
     end
   end

   FILES.each do |file|
     it "should encode #{File.basename(file)} call the passed block, passing it a String" do
       # we don't care about testing the stream subject as it has multiple JSON strings in it
       if File.basename(file) != 'twitter_stream.json'
         input = File.new(File.expand_path(file), 'r')
         encoder = Yajl::Encoder.new
         hash = Yajl::Parser.parse(input)
         output = ''
         encoder.encode(hash) do |json_str|
           output << json_str
         end
         hash2 = Yajl::Parser.parse(output)
         input.close
         expect(hash).to eq(hash2)
       end
     end
   end

  it "should encode with :pretty turned on and a single space indent, to an IO" do
    output = "{\n \"foo\": 1234\n}"
    obj = {:foo => 1234}
    io = StringIO.new
    encoder = Yajl::Encoder.new(:pretty => true, :indent => ' ')
    encoder.encode(obj, io)
    io.rewind
    expect(io.read).to eq(output)
  end

  it "should encode with :pretty turned on and a single space indent, and return a String" do
    output = "{\n \"foo\": 1234\n}"
    obj = {:foo => 1234}
    encoder = Yajl::Encoder.new(:pretty => true, :indent => ' ')
    output = encoder.encode(obj)
    expect(output).to eq(output)
  end

  it "should encode with :pretty turned on and a tab character indent, to an IO" do
    output = "{\n\t\"foo\": 1234\n}"
    obj = {:foo => 1234}
    io = StringIO.new
    encoder = Yajl::Encoder.new(:pretty => true, :indent => "\t")
    encoder.encode(obj, io)
    io.rewind
    expect(io.read).to eq(output)
  end

  it "should encode with :pretty turned on and a tab character indent, and return a String" do
    output = "{\n\t\"foo\": 1234\n}"
    obj = {:foo => 1234}
    encoder = Yajl::Encoder.new(:pretty => true, :indent => "\t")
    output = encoder.encode(obj)
    expect(output).to eq(output)
  end

  it "should encode with it's class method with :pretty and a tab character indent options set, to an IO" do
    output = "{\n\t\"foo\": 1234\n}"
    obj = {:foo => 1234}
    io = StringIO.new
    Yajl::Encoder.encode(obj, io, :pretty => true, :indent => "\t")
    io.rewind
    expect(io.read).to eq(output)
  end

  it "should encode with it's class method with :pretty and a tab character indent options set, and return a String" do
    output = "{\n\t\"foo\": 1234\n}"
    obj = {:foo => 1234}
    output = Yajl::Encoder.encode(obj, :pretty => true, :indent => "\t")
    expect(output).to eq(output)
  end

  it "should encode with it's class method with :pretty and a tab character indent options set, to a block" do
    output = "{\n\t\"foo\": 1234\n}"
    obj = {:foo => 1234}
    output = ''
    Yajl::Encoder.encode(obj, :pretty => true, :indent => "\t") do |json_str|
      output = json_str
    end
    expect(output).to eq(output)
  end

  it "should encode multiple objects into a single stream, to an IO" do
    io = StringIO.new
    obj = {:foo => 1234}
    encoder = Yajl::Encoder.new
    5.times do
      encoder.encode(obj, io)
    end
    io.rewind
    output = "{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}"
    expect(io.read).to eq(output)
  end

  it "should encode multiple objects into a single stream, and return a String" do
    obj = {:foo => 1234}
    encoder = Yajl::Encoder.new
    json_output = ''
    5.times do
      json_output << encoder.encode(obj)
    end
    output = "{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}{\"foo\":1234}"
    expect(json_output).to eq(output)
  end

  it "should encode all map keys as strings" do
    expect(Yajl::Encoder.encode({1=>1})).to eql("{\"1\":1}")
  end

  it "should check for and call #to_json if it exists on custom objects" do
    d = Dummy2.new
    expect(Yajl::Encoder.encode({:foo => d})).to eql('{"foo":{"hawtness":true}}')
  end

  it "should encode a hash where the key and value can be symbols" do
    expect(Yajl::Encoder.encode({:foo => :bar})).to eql('{"foo":"bar"}')
  end

  it "should encode using a newline or nil terminator" do
    expect(Yajl::Encoder.new(:terminator => "\n").encode({:foo => :bar})).to eql("{\"foo\":\"bar\"}\n")
    expect(Yajl::Encoder.new(:terminator => nil).encode({:foo => :bar})).to eql("{\"foo\":\"bar\"}")
  end

  it "should encode using a newline or nil terminator, to an IO" do
    s = StringIO.new
    Yajl::Encoder.new(:terminator => "\n").encode({:foo => :bar}, s)
    s.rewind
    expect(s.read).to eql("{\"foo\":\"bar\"}\n")

    s = StringIO.new
    Yajl::Encoder.new(:terminator => nil).encode({:foo => :bar}, s)
    s.rewind
    expect(s.read).to eql("{\"foo\":\"bar\"}")
  end

  it "should encode using a newline or nil terminator, using a block" do
    s = StringIO.new
    Yajl::Encoder.new(:terminator => "\n").encode({:foo => :bar}) do |chunk|
      s << chunk
    end
    s.rewind
    expect(s.read).to eql("{\"foo\":\"bar\"}\n")

    s = StringIO.new
    nilpassed = false
    Yajl::Encoder.new(:terminator => nil).encode({:foo => :bar}) do |chunk|
      nilpassed = true if chunk.nil?
      s << chunk
    end
    expect(nilpassed).to be_truthy
    s.rewind
    expect(s.read).to eql("{\"foo\":\"bar\"}")
  end

  it "should encode all integers correctly" do
    0.upto(129).each do |b|
      b = 1 << b
      [b, b-1, b-2, b+1, b+2].each do |i|
        expect(Yajl::Encoder.encode(i)).to eq(i.to_s)
        expect(Yajl::Encoder.encode(-i)).to eq((-i).to_s)
      end
    end
  end

  it "should not encode NaN" do
    expect {
      Yajl::Encoder.encode(0.0/0.0)
    }.to raise_error(Yajl::EncodeError)
  end

  it "should not encode Infinity or -Infinity" do
    expect {
      Yajl::Encoder.encode(1.0/0.0)
    }.to raise_error(Yajl::EncodeError)
    expect {
      Yajl::Encoder.encode(-1.0/0.0)
    }.to raise_error(Yajl::EncodeError)
  end

  it "should encode with unicode chars in the key" do
    hash = {"浅草" => "<- those are unicode"}
    expect(Yajl::Encoder.encode(hash)).to eql("{\"浅草\":\"<- those are unicode\"}")
  end

  if RUBY_VERSION =~ /^1.9/
    it "should return a string encoded in utf-8 if Encoding.default_internal is nil" do
      Encoding.default_internal = nil
      hash = {"浅草" => "<- those are unicode"}
      expect(Yajl::Encoder.encode(hash).encoding).to eql(Encoding.find('utf-8'))
    end

    it "should return a string encoded in utf-8 even if Encoding.default_internal *is* set" do
      Encoding.default_internal = Encoding.find('utf-8')
      hash = {"浅草" => "<- those are unicode"}
      expect(Yajl::Encoder.encode(hash).encoding).to eql(Encoding.default_internal)
      Encoding.default_internal = Encoding.find('us-ascii')
      hash = {"浅草" => "<- those are unicode"}
      expect(Yajl::Encoder.encode(hash).encoding).to eql(Encoding.find('utf-8'))
    end
  end

  it "should be able to escape / characters if html_safe is enabled" do
    unsafe_encoder = Yajl::Encoder.new(:html_safe => false)
    safe_encoder   = Yajl::Encoder.new(:html_safe => true)

    expect(unsafe_encoder.encode("</script>")).not_to eql("\"<\\/script>\"")
    expect(safe_encoder.encode("</script>")).to eql("\"<\\/script>\"")
  end

  it "should not encode characters with entities by default" do
    expect(Yajl.dump("\u2028\u2029><&")).to eql("\"\u2028\u2029><&\"")
  end

  it "should encode characters with entities when enabled" do
    expect(Yajl.dump("\u2028\u2029><&", entities: true)).to eql("\"\\u2028\\u2029\\u003E\\u003C\\u0026\"")
  end

  it "should default to *not* escaping / characters" do
    unsafe_encoder = Yajl::Encoder.new
    expect(unsafe_encoder.encode("</script>")).not_to eql("\"<\\/script>\"")
  end

  it "should encode slashes when enabled" do
    unsafe_encoder = Yajl::Encoder.new(:entities => false)
    safe_encoder   = Yajl::Encoder.new(:entities => true)

    expect(unsafe_encoder.encode("</script>")).not_to eql("\"<\\/script>\"")
    expect(safe_encoder.encode("</script>")).to eql("\"\\u003C\\/script\\u003E\"")
  end

  it "return value of #to_json must be a string" do
    expect {
      Yajl::Encoder.encode(TheMindKiller.new)
    }.to raise_error(TypeError)
  end

  it "return value of #to_s must be a string" do
    expect {
      if TheMindKillerDuce.send(:method_defined?, :to_json)
        TheMindKillerDuce.send(:undef_method, :to_json)
      end
      Yajl::Encoder.encode(TheMindKillerDuce.new)
    }.to raise_error(TypeError)
  end

  it "should raise an exception for deeply nested arrays" do
    root = []
    a = root
    (Yajl::MAX_DEPTH + 1).times { |_| a << []; a = a[0] }
    expect {
      Yajl::Encoder.encode(root)
    }.to raise_error(Yajl::EncodeError)
  end

  it "should raise an exception for deeply nested hashes" do
    root = {}
    a = root
    (Yajl::MAX_DEPTH + 1).times { |_| a["a"] = {}; a = a["a"] }
    expect {
      Yajl::Encoder.encode(root)
    }.to raise_error(Yajl::EncodeError)
  end
end

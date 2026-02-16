# encoding: UTF-8
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

describe "One-off JSON examples" do
  it "should not blow up with a bad surrogate trailer" do
    # https://github.com/brianmario/yajl-ruby/issues/176
    bad_json = "{\"e\":{\"\\uD800\\\\DC00\":\"a\"}}"

    Yajl::Parser.new.parse(bad_json)
  end

  it "should parse 23456789012E666 and return Infinity" do
    infinity = (1.0/0)
    silence_warnings do
      expect(Yajl::Parser.parse(StringIO.new('{"key": 23456789012E666}'))).to eq({"key" => infinity})
    end
  end

  it "should not parse JSON with a comment, with :allow_comments set to false" do
    json = StringIO.new('{"key": /* this is a comment */ "value"}')
    expect {
      Yajl::Parser.parse(json, :allow_comments => false)
    }.to raise_error(Yajl::ParseError)
  end

  it "should parse JSON with a comment, with :allow_comments set to true" do
    json = StringIO.new('{"key": /* this is a comment */ "value"}')
    expect {
      Yajl::Parser.parse(json, :allow_comments => true)
    }.not_to raise_error
  end

  it "should not parse invalid UTF8 with :check_utf8 set to true" do
    parser = Yajl::Parser.new(:check_utf8 => true)
    expect {
      parser.parse("[\"#{"\201\203"}\"]")
    }.to raise_error(Yajl::ParseError)
  end

  it "should parse invalid UTF8 with :check_utf8 set to false" do
    parser = Yajl::Parser.new(:check_utf8 => false)
    parser.parse("[\"#{"\201\203"}\"]").inspect
  end

  it "should parse using it's class method, from an IO" do
    io = StringIO.new('{"key": 1234}')
    expect(Yajl::Parser.parse(io)).to eq({"key" => 1234})
  end

  it "should parse using it's class method, from a string with symbolized keys" do
    expect(Yajl::Parser.parse('{"key": 1234}', :symbolize_keys => true)).to eq({:key => 1234})
  end

  it "should parse using it's class method, from a utf-8 string with multibyte characters, with symbolized keys" do
    expect(Yajl::Parser.parse('{"日本語": 1234}', :symbolize_keys => true)).to eq({:"日本語" => 1234})
  end

  it "should parse using it's class method, from a string" do
    expect(Yajl::Parser.parse('{"key": 1234}')).to eq({"key" => 1234})
  end

  it "should parse using it's class method, from a string with a block" do
    output = nil
    Yajl::Parser.parse('{"key": 1234}') do |obj|
      output = obj
    end
    expect(output).to eq({"key" => 1234})
  end

  it "should parse numbers greater than 2,147,483,648" do
    expect(Yajl::Parser.parse("{\"id\": 2147483649}")).to eql({"id" => 2147483649})
    expect(Yajl::Parser.parse("{\"id\": 5687389800}")).to eql({"id" => 5687389800})
    expect(Yajl::Parser.parse("{\"id\": 1046289770033519442869495707521600000000}")).to eql({"id" => 1046289770033519442869495707521600000000})
  end

  if RUBY_VERSION =~ /^1.9/
    it "should encode non-ascii symbols in utf-8" do
      parsed = Yajl::Parser.parse('{"曦": 1234}', :symbolize_keys => true)
      expect(parsed.keys.fetch(0).encoding).to eq(Encoding::UTF_8)
    end

    it "should return strings and hash keys in utf-8 if Encoding.default_internal is nil" do
      Encoding.default_internal = nil
      expect(Yajl::Parser.parse('{"key": "value"}').keys.first.encoding).to eql(Encoding.find('utf-8'))
      expect(Yajl::Parser.parse('{"key": "value"}').values.first.encoding).to eql(Encoding.find('utf-8'))
    end

    it "should return strings and hash keys encoded as specified in Encoding.default_internal if it's set" do
      Encoding.default_internal = Encoding.find('utf-8')
      expect(Yajl::Parser.parse('{"key": "value"}').keys.first.encoding).to eql(Encoding.default_internal)
      expect(Yajl::Parser.parse('{"key": "value"}').values.first.encoding).to eql(Encoding.default_internal)
      Encoding.default_internal = Encoding.find('us-ascii')
      expect(Yajl::Parser.parse('{"key": "value"}').keys.first.encoding).to eql(Encoding.default_internal)
      expect(Yajl::Parser.parse('{"key": "value"}').values.first.encoding).to eql(Encoding.default_internal)
    end
  end
end

# encoding: UTF-8
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

class Dummy; end

describe "JSON Gem compatability API" do
  it "shoud not mixin #to_json on base objects until compatability has been enabled" do
    d = Dummy.new

    expect(d.respond_to?(:to_json)).not_to be_truthy
    expect("".respond_to?(:to_json)).not_to be_truthy
    expect(1.respond_to?(:to_json)).not_to be_truthy
    expect("1.5".to_f.respond_to?(:to_json)).not_to be_truthy
    expect([].respond_to?(:to_json)).not_to be_truthy
    expect({:foo => "bar"}.respond_to?(:to_json)).not_to be_truthy
    expect(true.respond_to?(:to_json)).not_to be_truthy
    expect(false.respond_to?(:to_json)).not_to be_truthy
    expect(nil.respond_to?(:to_json)).not_to be_truthy
  end

  it "should mixin #to_json on base objects after compatability has been enabled" do
    require 'yajl/json_gem'
    d = Dummy.new

    expect(d.respond_to?(:to_json)).to be_truthy
    expect("".respond_to?(:to_json)).to be_truthy
    expect(1.respond_to?(:to_json)).to be_truthy
    expect("1.5".to_f.respond_to?(:to_json)).to be_truthy
    expect([].respond_to?(:to_json)).to be_truthy
    expect({:foo => "bar"}.respond_to?(:to_json)).to be_truthy
    expect(true.respond_to?(:to_json)).to be_truthy
    expect(false.respond_to?(:to_json)).to be_truthy
    expect(nil.respond_to?(:to_json)).to be_truthy
  end

  it "should require yajl/json_gem to enable the compatability API" do
    expect(defined?(JSON)).to be_truthy

    expect(JSON.respond_to?(:parse)).to be_truthy
    expect(JSON.respond_to?(:generate)).to be_truthy
    expect(JSON.respond_to?(:pretty_generate)).to be_truthy
    expect(JSON.respond_to?(:load)).to be_truthy
    expect(JSON.respond_to?(:dump)).to be_truthy
  end

  it "should allow default parsing options be set with JSON.default_options" do
    default = JSON.default_options[:symbolize_keys]
    expect(JSON.parse('{"foo": 1234}')).to be === {"foo" => 1234}
    JSON.default_options[:symbolize_keys] = true
    expect(JSON.parse('{"foo": 1234}')).to be === {:foo => 1234}
    JSON.default_options[:symbolize_keys] = default # ensure the rest of the test cases expect the default
  end

  it "should also allow the json gem's symbolize_names key" do
    expect(JSON.parse('{"foo": 1234}', :symbolize_names => true)).to be === {:foo => 1234}
  end

  it "should encode arbitrary classes via their default to_json method" do
    d = Dummy.new
    expect(d.to_json).to eq("\"#{d.to_s}\"")

    t = Time.now
    expect(t.to_json).to eq("\"#{t.to_s}\"")

    da = Date.today
    expect(da.to_json).to eq("\"#{da.to_s}\"")

    dt = DateTime.new
    expect(dt.to_json).to eq("\"#{dt.to_s}\"")
  end

  it "should have the standard parsing and encoding exceptions mapped" do
    expect(JSON::JSONError.new.is_a?(StandardError)).to be_truthy
    expect(JSON::ParserError.new.is_a?(JSON::JSONError)).to be_truthy
    expect(JSON::GeneratorError.new.is_a?(JSON::JSONError)).to be_truthy

    expect {
      JSON.parse("blah")
    }.to raise_error(JSON::ParserError)

    expect {
      JSON.generate(0.0/0.0)
    }.to raise_error(JSON::GeneratorError)
  end

  context "ported tests for Unicode" do
    it "should be able to encode and parse unicode" do
      expect('""').to eql(''.to_json)
      expect('"\\b"').to eql("\b".to_json)
      expect('"\u0001"').to eql(0x1.chr.to_json)
      expect('"\u001F"').to eql(0x1f.chr.to_json)
      expect('" "').to eql(' '.to_json)
      expect("\"#{0x7f.chr}\"").to eql(0x7f.chr.to_json)
      utf8 = [ "© ≠ €! \01" ]
      json = "[\"© ≠ €! \\u0001\"]"
      expect(json).to eql(utf8.to_json)
      expect(utf8).to eql(JSON.parse(json))
      utf8 = ["\343\201\202\343\201\204\343\201\206\343\201\210\343\201\212"]
      json = "[\"あいうえお\"]"
      expect(json).to eql(utf8.to_json)
      expect(utf8).to eql(JSON.parse(json))
      utf8 = ['საქართველო']
      json = "[\"საქართველო\"]"
      expect(json).to eql(utf8.to_json)
      expect(utf8).to eql(JSON.parse(json))
      expect('["Ã"]').to eql(JSON.generate(["Ã"]))
      expect(["€"]).to eql(JSON.parse('["\u20ac"]'))
      utf8_str = "\xf0\xa0\x80\x81"
      utf8 = [utf8_str]
      json = "[\"#{utf8_str}\"]"
      expect(json).to eql(JSON.generate(utf8))
      expect(utf8).to eql(JSON.parse(json))
    end
  end

  context "ported tests for generation" do
    before(:all) do
      @hash = {
        'a' => 2,
        'b' => 3.141,
        'c' => 'c',
        'd' => [ 1, "b", 3.14 ],
        'e' => { 'foo' => 'bar' },
        'g' => "blah",
        'h' => 1000.0,
        'i' => 0.001
      }

      @json2 = '{"a":2,"b":3.141,"c":"c","d":[1,"b",3.14],"e":{"foo":"bar"},"g":"blah","h":1000.0,"i":0.001}'

      @json3 = %{
        {
          "a": 2,
          "b": 3.141,
          "c": "c",
          "d": [1, "b", 3.14],
          "e": {"foo": "bar"},
          "g": "blah",
          "h": 1000.0,
          "i": 0.001
        }
      }.chomp
    end

    it "should be able to unparse" do
      json = JSON.generate(@hash)
      expect(JSON.parse(@json2)).to eq(JSON.parse(json))
      parsed_json = JSON.parse(json)
      expect(@hash).to eq(parsed_json)
      json = JSON.generate({1=>2})
      expect('{"1":2}').to eql(json)
      parsed_json = JSON.parse(json)
      expect({"1"=>2}).to eq(parsed_json)
    end

    it "should be able to unparse pretty" do
      json = JSON.pretty_generate(@hash)
      expect(JSON.parse(@json3)).to eq(JSON.parse(json))
      parsed_json = JSON.parse(json)
      expect(@hash).to eq(parsed_json)
      json = JSON.pretty_generate({1=>2})
      test = "{\n  \"1\": 2\n}".chomp
      expect(test).to eq(json)
      parsed_json = JSON.parse(json)
      expect({"1"=>2}).to eq(parsed_json)
    end
  end

  context "ported fixture tests" do
    fixtures = File.join(File.dirname(__FILE__), '../parsing/fixtures/*.json')
    passed, failed = Dir[fixtures].partition { |f| f['pass'] }
    JSON_PASSED = passed.inject([]) { |a, f| a << [ f, File.read(f) ] }.sort
    JSON_FAILED = failed.inject([]) { |a, f| a << [ f, File.read(f) ] }.sort

    JSON_FAILED.each do |name, source|
      it "should not be able to parse #{File.basename(name)} as an IO" do
          expect {
            JSON.parse(StringIO.new(source))
          }.to raise_error(JSON::ParserError)
      end
    end

    JSON_FAILED.each do |name, source|
      it "should not be able to parse #{File.basename(name)} as a string" do
          expect {
            JSON.parse(source)
          }.to raise_error(JSON::ParserError)
      end
    end

    JSON_PASSED.each do |name, source|
      it "should be able to parse #{File.basename(name)} as an IO" do
          expect {
            JSON.parse(StringIO.new(source))
          }.not_to raise_error
      end
    end

    JSON_PASSED.each do |name, source|
      it "should be able to parse #{File.basename(name)} as a string" do
          expect {
            JSON.parse(source)
          }.not_to raise_error
      end
    end
  end
end

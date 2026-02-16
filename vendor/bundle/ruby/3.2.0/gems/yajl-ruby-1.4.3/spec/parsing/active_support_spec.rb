# encoding: UTF-8
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

describe "ActiveSupport test cases" do
  TESTS = {
    %q({"returnTo":{"\/categories":"\/"}})        => {"returnTo" => {"/categories" => "/"}},
    %q({"return\\"To\\":":{"\/categories":"\/"}}) => {"return\"To\":" => {"/categories" => "/"}},
    %q({"returnTo":{"\/categories":1}})          => {"returnTo" => {"/categories" => 1}},
    %({"returnTo":[1,"a"]})                    => {"returnTo" => [1, "a"]},
    %({"returnTo":[1,"\\"a\\",", "b"]})        => {"returnTo" => [1, "\"a\",", "b"]},
    %({"a": "'", "b": "5,000"})                  => {"a" => "'", "b" => "5,000"},
    %({"a": "a's, b's and c's", "b": "5,000"})   => {"a" => "a's, b's and c's", "b" => "5,000"},
    # multibyte
    %({"matzue": "松江", "asakusa": "浅草"}) => {"matzue" => "松江", "asakusa" => "浅草"},
    %({"a": "2007-01-01"})                       => {'a' => "2007-01-01"},
    %({"a": "2007-01-01 01:12:34 Z"})            => {'a' => "2007-01-01 01:12:34 Z"},
    # no time zone
    %({"a": "2007-01-01 01:12:34"})              => {'a' => "2007-01-01 01:12:34"},
    # needs to be *exact*
    %({"a": " 2007-01-01 01:12:34 Z "})          => {'a' => " 2007-01-01 01:12:34 Z "},
    %({"a": "2007-01-01 : it's your birthday"})  => {'a' => "2007-01-01 : it's your birthday"},
    %([])    => [],
    %({})    => {},
    %({"a":1})     => {"a" => 1},
    %({"a": ""})    => {"a" => ""},
    %({"a":"\\""}) => {"a" => "\""},
    %({"a": null})  => {"a" => nil},
    %({"a": true})  => {"a" => true},
    %({"a": false}) => {"a" => false},
    %q({"a": "http:\/\/test.host\/posts\/1"}) => {"a" => "http://test.host/posts/1"},
    %q({"a": "\u003cunicode\u0020escape\u003e"}) => {"a" => "<unicode escape>"},
    %q({"a": "\\\\u0020skip double backslashes"}) => {"a" => "\\u0020skip double backslashes"},
    %q({"a": "\u003cbr /\u003e"}) => {'a' => "<br />"},
    %q({"b":["\u003ci\u003e","\u003cb\u003e","\u003cu\u003e"]}) => {'b' => ["<i>","<b>","<u>"]}
  }

  TESTS.each do |json, expected|
    it "should be able to parse #{json} as an IO" do
      expect {
        expect(Yajl::Parser.parse(StringIO.new(json))).to eq(expected)
      }.not_to raise_error
    end
  end

  TESTS.each do |json, expected|
    it "should be able to parse #{json} as a string" do
      expect {
        expect(Yajl::Parser.parse(json)).to eq(expected)
      }.not_to raise_error
    end
  end

  it "should fail parsing {: 1} as an IO" do
    expect {
      Yajl::Parser.parse(StringIO.new("{: 1}"))
    }.to raise_error(Yajl::ParseError)
  end

  it "should fail parsing {: 1} as a string" do
    expect {
      Yajl::Parser.parse("{: 1}")
    }.to raise_error(Yajl::ParseError)
  end
end

require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

require 'stringio'
require 'json'

describe "projection" do
  it "should work" do
    stream = StringIO.new('{"name": "keith", "age": 27}')
    projector = Yajl::Projector.new(stream)
    projection = projector.project({"name" => nil})
    expect(projection['name']).to eql("keith")
  end

  it "should filter" do
    stream = StringIO.new('{"name": "keith", "age": 27}')
    projector = Yajl::Projector.new(stream)
    projection = projector.project({"name" => nil})
    expect(projection['age']).to eql(nil)
  end

  it "should raise an exception and not leak memory" do
    stream = StringIO.new('foo')
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(Yajl::ParseError)
  end

  it "should raise an exception and not segv" do
    stream = StringIO.new('[,,,,]')
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(Yajl::ParseError)
  end

  it "should raise an exception and not segv on colons" do
    stream = StringIO.new('[::::]')
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(Yajl::ParseError)
  end

  it "should behave the same way as the regular parser on bad tokens like comma" do
    bad_json = '{"name": "keith", "age":, 27}'
    stream = StringIO.new(bad_json)
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(capture_exception_for(bad_json).class)
  end

  it "should behave the same way as the regular parser on bad tokens like colon" do
    bad_json = '{"name": "keith", "age":: 27}'
    stream = StringIO.new(bad_json)
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(capture_exception_for(bad_json).class)
  end

  it "should behave the same way as the regular parser on not enough json" do
    bad_json = '{"name": "keith", "age":'
    stream = StringIO.new(bad_json)
    projector = Yajl::Projector.new(stream)
    expect {
      projector.project({"name" => nil})
    }.to raise_error(capture_exception_for(bad_json).class)
  end

  def capture_exception_for(bad_json)
    Yajl::Parser.new.parse(bad_json)
  rescue Exception => e
    e
  end

  def project(schema, over: "", json: nil, stream: nil)
    if stream.nil?
      if json.nil?
        json = over.to_json
      end

      stream = StringIO.new(json)
    end

    Yajl::Projector.new(stream).project(schema)
  end

  it "filters arrays" do
    json = {
      "users" => [
        {
          "name" => "keith",
          "company" => "internet plumbing inc",
          "department" => "janitorial",
        },
        {
          "name" => "justin",
          "company" => "big blue",
          "department" => "programming?",
        },
        {
          "name" => "alan",
          "company" => "different colour of blue",
          "department" => "drop bear containment",
        }
      ]
    }.to_json

    puts json

    schema = {
      # /users is an array of objects, each having many keys we only want name
      "users" => {
        "name" => nil,
      }
    }

    expect(project(schema, json: json)).to eql({
      "users" => [
        { "name" => "keith" },
        { "name" => "justin" },
        { "name" => "alan" }
      ]
    })
  end

  it "filters top level arrays" do
    json = [
      {
        "name" => "keith",
        "personal detail" => "thing",
      },
      {
        "name" => "cory",
        "phone number" => "unknown",
      }
    ]

    schema = {
      "name" => nil,
    }

    expect(project(schema, over: json)).to eql([
      { "name" => "keith" },
      { "name" => "cory" },
    ])
  end

  it "filters nested schemas" do
    json = {
      "foo" => 42,

      "bar" => {
        "name" => "keith",
        "occupation" => "professional computering",
        "age" => 26,
        "hobbies" => [
          "not computering",
        ]
      },

      "qux" => {
        "quux" => [
          {
            "name" => "Reactive X",
            "members" => "many",
          },
          {
            "name" => "lstoll",
            "members" => "such",
          },
          {
            "name" => "github",
            "members" => "very",
          },
          {
            "name" => "theleague",
            "members" => "numerous",
          }
        ],

        "corge" => {
          "name" => "Brighton",
          "address" =>"Buckingham Road",
        },
      },

      "grault" => nil,

      "waldo" => true,
    }

    schema = {
      # include the /foo subtree (is a single number)
      "foo" => nil,

      # ignore the bar subtree (is an object)
      # "bar" => ???

      # include some of the /qux subtree (is an object)
      "qux" => {
        # include the whole /qux/quux subtree (is an array of objects)
        "quux" => nil,

        # include some of the /qux/corge subtree (is another object)
        "corge" => {
          # include name (is a string)
          "name" => nil,
          # include age (is missing from source doc)
          "age" => nil,
          # ignore address
          # "address" => ???
        },
      },

      # include the /grault subtree (is a null literal)
      "grault" => nil,

      # include the /waldo subtree (is a boolean literal)
      "waldo" => nil,
    }

    expect(project(schema, over: json)).to eql({
      "foo" => 42,

      "qux" => {
        "quux" => [
          {
            "name" => "Reactive X",
            "members" => "many",
          },
          {
            "name" => "lstoll",
            "members" => "such",
          },
          {
            "name" => "github",
            "members" => "very",
          },
          {
            "name" => "theleague",
            "members" => "numerous",
          }
        ],

        "corge" => {
          "name" => "Brighton",
        },
      },

      "grault" => nil,

      "waldo" => true,
    })
  end

  it "supports incompatible schemas" do
    json = {
      # surprise! the json doesn't include an object under the foo key
      "foo" => 42,
    }

    schema = {
      # include some of the /foo subtree
      "foo" => {
        # include the whole /foo/baz subtree
        "baz" => nil,
      }
    }

    # expect the 42 to be pulled out
    expect(project(schema, over: json)).to eql({
      "foo" => 42
    })
  end

  it "supports nil schema" do
    json = {
      "foo" => "bar",
    }

    expect(project(nil, over: json)).to eql({
      "foo" => "bar"
    })
  end

  it "supports empty schema" do
    json = {
      "foo" => "bar",
    }
    expect(project({}, over: json)).to eql({})
  end

  it "supports object projection" do
    json = {
      "foo" => "bar",
      "qux" => "quux",
    }

    schema = {
      "foo" => nil,
    }

    expect(project(schema, over: json)).to eql({
      "foo" => "bar"
    })
  end

  it "projects the readme example" do
    json = <<-EOJ
    [
      {
        "user": {
          "name": "keith",
          "age": 26,
          "jobs": [
            {
              "title": "director of overworking",
              "company": "south coast software",
              "department": "most"
            },
            {
              "title": "some kind of computering",
              "company": "github the website dot com",
              "department": true
            }
          ]
        },
        "another key": {

        },
        "woah this document is huge": {

        },
        "many megabytes": {

        },
        "etc": {

        }
      }
    ]
EOJ

    schema = {
      "user" => {
        "name" => nil,
        "jobs" => {
          "title" => nil,
        },
      },
    }

    expect(project(schema, json: json)).to eql([{
      "user" => {
        "name" => "keith",
        "jobs" => [
          { "title" => "director of overworking" },
          { "title" => "some kind of computering" },
        ]
      }
    }])
  end

  it "errors with invalid json" do
    expect {
      project({"b" => nil}, json: '{"a":, "b": 2}')
    }.to raise_error(StandardError)
  end

  it "errors with ignored unbalanced object syntax" do
    expect {
      project({"b" => nil}, json: '{"a": {{, "b": 2}')
    }.to raise_error(StandardError)
  end

  it "errors with accepted unbalanced object tokens" do
    expect {
      project({"a" => nil}, json: '{"a": {"b": 2}')
    }.to raise_error(Yajl::ParseError)
  end

  it "errors when projecting if an object comma is missing" do
    expect {
      project({"a" => nil}, json: '{"a": 1 "b": 2}')
    }.to raise_error(Yajl::ParseError)
  end

  it "errors when building if an object comma is missing" do
    expect {
      project(nil, json: '{"a": {"b": 2 "c": 3}}')
    }.to raise_error(Yajl::ParseError)
  end

  it "errors when eof instead of simple value" do
    expect {
      project(nil, json: '[')
    }.to raise_error(Yajl::ParseError)
  end

  it "errors when arrays don't have a comma between elements" do
    expect {
      project(nil, json: '[1 2]')
    }.to raise_error(Yajl::ParseError)
  end

  it "supports parsing empty array" do
    expect(project(nil, json: '[]')).to eql([])
  end

  it "supports parsing empty object" do
    expect(project(nil, json: '{}')).to eql({})
  end

  it "reads a full buffer" do
    json = "[" + "1,"*2046 + "1 ]"
    expect(json.size).to eql(4096)
    expect(project(nil, json: json)).to eql(Array.new(2047, 1))
  end

  it "reads into a second buffer" do
    json = "[" + "1,"*2047 + "1 ]"
    expect(json.size).to eql(4098)
    expect(JSON.parse(json)).to eql(Array.new(2048, 1))
    expect(project(nil, json: json)).to eql(Array.new(2048, 1))
  end

  it "supports parsing big strings" do
    json = [
      "a",
      "b"*10_000,
      "c",
    ]
    expect(project(nil, over: json)).to eql(json)
  end

  it "supports bigger read buffers" do
    json = {
      "a"*10_000 => "b"*10_000
    }.to_json
    stream = StringIO.new(json)
    expect(Yajl::Projector.new(stream, 8192).project(nil)).to have_key("a"*10_000)
  end

  it "errors if starting with closing object" do
    expect {
      project(nil, json: '}')
    }.to raise_error(Yajl::ParseError)
  end

  it "handles objects with utf16 escape sequences as keys" do
    projection = project(nil, json: '{"\ud83d\ude00": "grinning face"}')
    literal = {"😀" => "grinning face"}
    expect(projection).to eql(literal)
  end

  it "handles objects with non-ascii utf8 bytes as keys" do
    expect(project(nil, json: '{"😀": "grinning face"}')).to eql({"😀" => "grinning face"})
  end

  it "handles strings with utf16 escape sequences as object values" do
    expect(project(nil, json: '{"grinning face": "\ud83d\ude00"}')).to eql({"grinning face" => "😀"})
  end

  it "handles strings with utf16 escape sequences as array values" do
    projection = project(nil, json: '["\ud83d\ude00"]')
    puts projection.first.inspect
    puts projection.first.bytes

    literal = ["😀"]
    puts literal.first.inspect
    puts literal.first.bytes

    expect(projection).to eql(literal)
  end

  it "handles strings with non-ascii utf8 bytes as array values" do
    projection = project(nil, json: '["😀"]')
    puts projection.first.inspect
    puts projection.first.bytes

    literal = ["😀"]
    puts literal.first.inspect
    puts literal.first.bytes

    expect(projection).to eql(literal)
  end

  it "ignores strings with utf16 escape sequences" do
    expect(project({"grinning face with open mouth" => nil}, json: '{"grinning face": "\ud83d\ude00", "grinning face with open mouth": "\ud83d\ude03"}')).to eql({"grinning face with open mouth" => "😃"})
  end

  it "handles objects whose second key has escape sequences" do
    expect(project(nil, json: '{"foo": "bar", "\ud83d\ude00": "grinning face"}')).to eql({"foo" => "bar", "😀" => "grinning face"})
  end
end

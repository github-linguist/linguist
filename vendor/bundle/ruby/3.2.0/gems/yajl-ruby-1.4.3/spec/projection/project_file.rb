require File.expand_path(File.dirname(__FILE__) + '/../spec_helper.rb')

require 'benchmark'
require 'benchmark/memory'

describe "file projection" do
  it "projects file streams" do
    schema = {
      "forced" => nil,
      "created" => nil,
      "pusher" => {
        "name" => nil,
      },
      "repository" => {
        "name" => nil,
        "full_name" => nil,
      },
      "ref" => nil,
      "compare" => nil,
      "commits" => {
        "distinct" => nil,
        "message" => nil,
        "url" => nil,
        "id" => nil,
        "author" => {
          "username" => nil,
        }
      }
    }

    file_path = ENV['JSON_FILE']
    if file_path.nil? || file_path.empty?
      return
    end

    Benchmark.memory { |x|
      x.report("project (yajl)") { Yajl::Projector.new(File.open(file_path, 'r')).project(schema) }
      x.compare!
    }
  end
end

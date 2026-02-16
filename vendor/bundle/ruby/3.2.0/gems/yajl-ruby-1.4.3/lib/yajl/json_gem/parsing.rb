require 'yajl' unless defined?(Yajl::Parser)

module JSON
  class JSONError < StandardError; end unless defined?(JSON::JSONError)
  class ParserError < JSONError; end unless defined?(JSON::ParserError)

  def self.default_options
    @default_options ||= {:symbolize_keys => false}
  end

  def self.parse(str, opts=JSON.default_options)
    begin
      Yajl::Parser.parse(str, opts)
    rescue Yajl::ParseError => e
      raise JSON::ParserError, e.message
    end
  end

  def self.load(input, *args)
    begin
      Yajl::Parser.parse(input, default_options)
    rescue Yajl::ParseError => e
      raise JSON::ParserError, e.message
    end
  end
end
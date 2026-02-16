# frozen-string-literal: true

require 'time'
require 'stringio'
require 'tomlrb/version'
require 'tomlrb/local_date_time'
require 'tomlrb/local_date'
require 'tomlrb/local_time'
require 'tomlrb/string_utils'
require 'tomlrb/scanner'
require 'tomlrb/parser'
require 'tomlrb/handler'

module Tomlrb
  class ParseError < StandardError; end

  # Parses a valid TOML string into its Ruby data structure
  #
  # @param string_or_io [String, StringIO] the content
  # @param options [Hash] the options hash
  # @option options [Boolean] :symbolize_keys (false) whether to return the keys as symbols or strings
  # @return [Hash] the Ruby data structure represented by the input
  def self.parse(string_or_io, **options)
    io = string_or_io.is_a?(String) ? StringIO.new(string_or_io) : string_or_io
    scanner = Scanner.new(io)
    parser = Parser.new(scanner, **options)
    begin
      handler = parser.parse
    rescue Racc::ParseError => e
      raise ParseError, e.message
    end

    handler.output
  end

  # Reads a file content and parses it into its Ruby data structure
  #
  # @param path [String] the path to the file
  # @param options [Hash] the options hash
  # @option options [Boolean] :symbolize_keys (false) whether to return the keys as symbols or strings
  # @return [Hash] the Ruby data structure represented by the input
  def self.load_file(path, **options)
    # By default Ruby sets the external encoding of an IO object to the
    # default external encoding. The default external encoding is set by
    # locale encoding or the interpreter -E option.
    tmp = File.read(path, encoding: 'utf-8')
    Tomlrb.parse(tmp, **options)
  end
end

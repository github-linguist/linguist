require 'mocha/parameter_matchers/base'
require 'mocha/parameter_matchers/deprecations'
require 'uri'
require 'cgi'

module Mocha
  module ParameterMatchers
    module Methods
      # Matches a URI without regard to the ordering of parameters in the query string.
      #
      # @param [String] uri URI to match.
      # @return [EquivalentUri] parameter matcher.
      #
      # @see Expectation#with
      #
      # @example Actual URI is equivalent.
      #   object = mock()
      #   object.expects(:method_1).with(equivalent_uri('http://example.com/foo?a=1&b=2))
      #   object.method_1('http://example.com/foo?b=2&a=1')
      #   # no error raised
      #
      # @example Actual URI is not equivalent.
      #   object = mock()
      #   object.expects(:method_1).with(equivalent_uri('http://example.com/foo?a=1&b=2))
      #   object.method_1('http://example.com/foo?a=1&b=3')
      #   # error raised, because the query parameters were different
      def equivalent_uri(uri)
        EquivalentUri.new(uri)
      end
    end

    define_deprecated_matcher_method(:equivalent_uri)

    # Parameter matcher which matches URIs with equivalent query strings.
    class EquivalentUri
      include BaseMethods

      # @private
      def initialize(uri)
        @uri = URI.parse(uri)
      end

      # @private
      def matches?(available_parameters)
        actual = explode(URI.parse(available_parameters.shift))
        expected = explode(@uri)
        actual == expected
      end

      # @private
      def mocha_inspect
        "equivalent_uri(#{@uri.mocha_inspect})"
      end

      private

      # @private
      def explode(uri)
        query_hash = CGI.parse(uri.query || '')
        URI::Generic::COMPONENT.inject({}) { |h, k| h.merge(k => uri.__send__(k)) }.merge(query: query_hash)
      end
    end

    provide_deprecated_access_to(:EquivalentUri)
  end
end

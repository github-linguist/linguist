require 'mime/types'
require 'yaml'

module Linguist
  module Mime
    # Internal: Look up mime type for extension.
    #
    # ext - The extension String. May include leading "."
    #
    # Examples
    #
    #   Mime.mime_for('.html')
    #   # => 'text/html'
    #
    #   Mime.mime_for('txt')
    #   # => 'text/plain'
    #
    # Return mime type String otherwise falls back to 'text/plain'.
    def self.mime_for(ext)
      mime_type = lookup_mime_type_for(ext)
      mime_type ? mime_type.to_s : 'text/plain'
    end

    # Internal: Lookup mime type for extension or mime type
    #
    # ext_or_mime_type - A file extension ".txt" or mime type "text/plain".
    #
    # Returns a MIME::Type
    def self.lookup_mime_type_for(ext_or_mime_type)
      ext_or_mime_type ||= ''

      if ext_or_mime_type =~ /\w+\/\w+/
        guesses = ::MIME::Types[ext_or_mime_type]
      else
        guesses = ::MIME::Types.type_for(ext_or_mime_type)
      end

      # Prefer text mime types over binary
      guesses.detect { |type| type.ascii? } ||

        # Otherwise use the first guess
        guesses.first
    end
  end
end

require 'mime/types'
require 'yaml'

# Register additional mime type extensions
#
# Follows same format as mime-types data file
#   https://github.com/halostatue/mime-types/blob/master/lib/mime/types.rb.data
File.read(File.expand_path("../mimes.yml", __FILE__)).lines.each do |line|
  # Regexp was cargo culted from mime-types lib
  next unless line =~ %r{^
    #{MIME::Type::MEDIA_TYPE_RE}
    (?:\s@([^\s]+))?
    (?:\s:(#{MIME::Type::ENCODING_RE}))?
  }x

  mediatype  = $1
  subtype    = $2
  extensions = $3
  encoding   = $4

  # Lookup existing mime type
  mime_type = MIME::Types["#{mediatype}/#{subtype}"].first ||
    # Or create a new instance
    MIME::Type.new("#{mediatype}/#{subtype}")

  if extensions
    extensions.split(/,/).each do |extension|
      mime_type.extensions << extension
    end
  end

  if encoding
    mime_type.encoding = encoding
  end

  # Kind of hacky, but we need to reindex the mime type after making changes
  MIME::Types.add_type_variant(mime_type)
  MIME::Types.index_extensions(mime_type)
end

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

require 'mime/types'
require 'yaml'

# Register additional mime type extensions
File.read(File.expand_path("../mimes.yml", __FILE__)).lines.each do |line|
  next unless line =~ %r{^
    #{MIME::Type::MEDIA_TYPE_RE}
    (?:\s@([^\s]+))?
    (?:\s:(#{MIME::Type::ENCODING_RE}))?
  }x

  mediatype  = $1
  subtype    = $2
  extensions = $3
  encoding   = $4

  mime_type = MIME::Types["#{mediatype}/#{subtype}"].first ||
    MIME::Type.new("#{mediatype}/#{subtype}")

  if extensions
    extensions.split(/,/).each do |extension|
      if extension =~ /^-(\w+)/
        extension = $1
        mime_type.extensions.delete(extension)

        MIME::Types.instance_eval do
          @__types__.instance_eval do
            @extension_index[extension].delete(mime_type)
          end
        end
      else
        mime_type.extensions << extension
      end
    end
  end

  mime_type.encoding = encoding

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
      mime_type ? mime_type.simplified : 'text/plain'
    end

    # Internal: Determine if extension or mime type is binary.
    #
    # ext_or_mime_type - A file extension ".txt" or mime type "text/plain".
    #
    # Returns true or false
    def self.binary?(ext_or_mime_type)
      mime_type = lookup_mime_type_for(ext_or_mime_type)
      mime_type ? mime_type.binary? : false
    end

    # Internal: Lookup mime type for extension or mime type
    #
    # Returns a MIME::Type
    def self.lookup_mime_type_for(ext_or_mime_type)
      ext_or_mime_type ||= ''

      if ext_or_mime_type =~ /\w+\/\w+/
        guesses = ::MIME::Types[ext_or_mime_type]
      else
        guesses = ::MIME::Types.type_for(ext_or_mime_type)
      end

      guesses.first
    end
  end
end

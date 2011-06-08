require 'mime/types'
require 'yaml'

# Register additional mime type extensions
mime_extensions = YAML.load_file(File.expand_path("../mimes.yml", __FILE__))
mime_extensions.each do |mime_type, options|
  mime = MIME::Types[mime_type].first || MIME::Type.new(mime_type)

  (options['extensions'] || []).each { |ext| mime.extensions << ext }

  (options['exclude_extensions'] || []).each do |ext|
    mime.extensions.delete(ext)

    MIME::Types.instance_eval do
      @__types__.instance_eval do
        @extension_index[ext].delete(mime)
      end
    end
  end

  mime.binary  = options['binary']    if options.key?('binary')
  mime.encoding = options['encoding'] if options.key?('encoding')

  MIME::Types.add_type_variant(mime)
  MIME::Types.index_extensions(mime)
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

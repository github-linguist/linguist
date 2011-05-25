require 'mime/types'
require 'yaml'

# Register additional mime type extensions
mime_extensions = YAML.load_file(File.expand_path("../mimes.yml", __FILE__))
mime_extensions.each do |mime_type, exts|
  mime = MIME::Types[mime_type].first
  exts.each { |ext| mime.extensions << ext }
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
      ext ||= ''
      guesses = ::MIME::Types.type_for(ext)
      guesses.first ? guesses.first.simplified : 'text/plain'
    end

    Special = YAML.load_file(File.expand_path("../content_types.yml", __FILE__))

    # Internal: Look up Content-Type header to serve for extension.
    #
    # This value is used when serving raw blobs.
    #
    #   /github/linguist/raw/master/lib/linguist/mime.rb
    #
    # ext - The extension String. May include leading "."
    #
    #   Mime.content_type_for('.html')
    #   # => 'text/plain; charset=utf-8'
    #
    # Return Content-Type String otherwise falls back to
    # 'text/plain; charset=utf-8'.
    def self.content_type_for(ext)
      ext ||= ''

      # Lookup mime type
      type = mime_for(ext)

      # Substitute actual mime type if an override exists in content_types.yml
      type = Special[type] || Special[ext.sub(/^\./, '')] || type

      # Append default charset to text files
      type += '; charset=utf-8' if type =~ /^text\//

      type
    end
  end
end

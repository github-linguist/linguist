require 'mime/types'

# Register additional binary extensions
binary = MIME::Types['application/octet-stream'].first
binary.extensions << 'dmg'
binary.extensions << 'dll'
MIME::Types.index_extensions(binary)

# Register 'ear' and 'war' as java
java = MIME::Types['application/java-archive'].first
java.extensions << 'ear'
java.extensions << 'war'
MIME::Types.index_extensions(java)

module Linguist
  module Mime
    Special = YAML.load_file(File.expand_path("../content_types.yml", __FILE__))

    def self.mime_for(ext)
      ext ||= ''
      guesses = ::MIME::Types.type_for(ext)
      guesses.first ? guesses.first.simplified : 'text/plain'
    end

    def self.content_type_for(ext)
      ext ||= ''
      type = mime_for(ext)
      type = Special[type] || Special[ext.sub(/^\./, '')] || type
      type += '; charset=utf-8' if type =~ /^text\//
      type
    end
  end
end

require 'mime/types'
require 'yaml'

# Register additional mime type extensions
YAML.load_file(File.expand_path("../mimes.yml", __FILE__)).each do |mime_type, exts|
  mime = MIME::Types[mime_type].first
  exts.each { |ext| mime.extensions << ext }
  MIME::Types.index_extensions(mime)
end

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

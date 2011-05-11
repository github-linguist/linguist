require 'mime/types'

module Linguist
  module Mime
    Special = YAML.load_file(File.expand_path("../special_mime_types.yml", __FILE__))

    def self.lookup(ext)
      ext ||= ''

      guesses = ::MIME::Types.type_for(ext)
      orginal_type = guesses.first ? guesses.first.simplified : 'text/plain'

      type = Special[orginal_type] ||
        Special[ext.sub(/^\./, '')] ||
        orginal_type

      type += '; charset=utf-8' if type =~ /^text\//

      type
    end
  end
end

require 'mime/types'
require 'yaml'

module Linguist
  module Mime
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

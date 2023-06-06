require 'linguist/generated'
require 'cgi'
require 'charlock_holmes'
require 'mini_mime'
require 'yaml'

module Linguist
  # DEPRECATED Avoid mixing into Blob classes. Prefer functional interfaces
  # like `Linguist.detect` over `Blob#language`. Functions are much easier to
  # cache and compose.
  #
  # Avoid adding additional bloat to this module.
  #
  # BlobHelper is a mixin for Blobish classes that respond to "name",
  # "data" and "size" such as Grit::Blob.
  module BlobHelper
    # Public: Get the extname of the path
    #
    # Examples
    #
    #   blob(name='foo.rb').extname
    #   # => '.rb'
    #
    # Returns a String
    def extname
      File.extname(name.to_s)
    end

    # Internal: Lookup mime type for filename.
    #
    # Returns a MIME::Type
    def _mime_type
      if defined? @_mime_type
        @_mime_type
      else
        @_mime_type = MiniMime.lookup_by_filename(name.to_s)
      end
    end

    # Public: Get the actual blob mime type
    #
    # Examples
    #
    #   # => 'text/plain'
    #   # => 'text/html'
    #
    # Returns a mime type String.
    def mime_type
      _mime_type ? _mime_type.content_type : 'text/plain'
    end

    # Internal: Is the blob binary according to its mime type
    #
    # Return true or false
    def binary_mime_type?
      _mime_type ? _mime_type.binary? : false
    end

    # Internal: Is the blob binary according to its mime type,
    # overriding it if we have better data from the languages.yml
    # database.
    #
    # Return true or false
    def likely_binary?
      binary_mime_type? && !Language.find_by_filename(name)
    end

    # Public: Get the Content-Type header value
    #
    # This value is used when serving raw blobs.
    #
    # Examples
    #
    #   # => 'text/plain; charset=utf-8'
    #   # => 'application/octet-stream'
    #
    # Returns a content type String.
    def content_type
      @content_type ||= (binary_mime_type? || binary?) ? mime_type :
        (encoding ? "text/plain; charset=#{encoding.downcase}" : "text/plain")
    end

    # Public: Get the Content-Disposition header value
    #
    # This value is used when serving raw blobs.
    #
    #   # => "attachment; filename=file.tar"
    #   # => "inline"
    #
    # Returns a content disposition String.
    def disposition
      if text? || image?
        'inline'
      elsif name.nil?
        "attachment"
      else
        "attachment; filename=#{CGI.escape(name)}"
      end
    end

    def encoding
      if hash = detect_encoding
        hash[:encoding]
      end
    end

    def ruby_encoding
      if hash = detect_encoding
        hash[:ruby_encoding]
      end
    end

    # Try to guess the encoding
    #
    # Returns: a Hash, with :encoding, :confidence, :type
    #          this will return nil if an error occurred during detection or
    #          no valid encoding could be found
    def detect_encoding
      @detect_encoding ||= CharlockHolmes::EncodingDetector.new.detect(data) if data
    end

    # Public: Is the blob binary?
    #
    # Return true or false
    def binary?
      # Large blobs aren't even loaded into memory
      if data.nil?
        true

      # Treat blank files as text
      elsif data == ""
        false

      # Charlock doesn't know what to think
      elsif encoding.nil?
        true

      # If Charlock says its binary
      else
        detect_encoding[:type] == :binary
      end
    end

    # Public: Is the blob empty?
    #
    # Return true or false
    def empty?
      data.nil? || data == ""
    end

    # Public: Is the blob text?
    #
    # Return true or false
    def text?
      !binary?
    end

    # Public: Is the blob a supported image format?
    #
    # Return true or false
    def image?
      ['.png', '.jpg', '.jpeg', '.gif'].include?(extname.downcase)
    end

    # Public: Is the blob a supported 3D model format?
    #
    # Return true or false
    def solid?
      extname.downcase == '.stl'
    end

    # Public: Is this blob a CSV file?
    #
    # Return true or false
    def csv?
      text? && extname.downcase == '.csv'
    end

    # Public: Is the blob a PDF?
    #
    # Return true or false
    def pdf?
      extname.downcase == '.pdf'
    end

    MEGABYTE = 1024 * 1024

    # Public: Is the blob too big to load?
    #
    # Return true or false
    def large?
      size.to_i > MEGABYTE
    end

    # Public: Is the blob safe to colorize?
    #
    # Return true or false
    def safe_to_colorize?
      !large? && text? && !high_ratio_of_long_lines?
    end

    # Internal: Does the blob have a ratio of long lines?
    #
    # Return true or false
    def high_ratio_of_long_lines?
      return false if loc == 0
      size / loc > 5000
    end

    # Public: Is the blob viewable?
    #
    # Non-viewable blobs will just show a "View Raw" link
    #
    # Return true or false
    def viewable?
      !large? && text?
    end

    vendored_paths = YAML.load_file(File.expand_path("../vendor.yml", __FILE__))
    VendoredRegexp = Regexp.new(vendored_paths.join('|'))

    # Public: Is the blob in a vendored directory?
    #
    # Vendored files are ignored by language statistics.
    #
    # See "vendor.yml" for a list of vendored conventions that match
    # this pattern.
    #
    # Return true or false
    def vendored?
      path =~ VendoredRegexp ? true : false
    end

    documentation_paths = YAML.load_file(File.expand_path("../documentation.yml", __FILE__))
    DocumentationRegexp = Regexp.new(documentation_paths.join('|'))

    # Public: Is the blob in a documentation directory?
    #
    # Documentation files are ignored by language statistics.
    #
    # See "documentation.yml" for a list of documentation conventions that match
    # this pattern.
    #
    # Return true or false
    def documentation?
      path =~ DocumentationRegexp ? true : false
    end

    # Public: Get each line of data
    #
    # Requires Blob#data
    #
    # Returns an Array of lines
    def lines
      @lines ||=
        if viewable? && data
          # `data` is usually encoded as ASCII-8BIT even when the content has
          # been detected as a different encoding. However, we are not allowed
          # to change the encoding of `data` because we've made the implicit
          # guarantee that each entry in `lines` is encoded the same way as
          # `data`.
          #
          # Instead, we re-encode each possible newline sequence as the
          # detected encoding, then force them back to the encoding of `data`
          # (usually a binary encoding like ASCII-8BIT). This means that the
          # byte sequence will match how newlines are likely encoded in the
          # file, but we don't have to change the encoding of `data` as far as
          # Ruby is concerned. This allows us to correctly parse out each line
          # without changing the encoding of `data`, and
          # also--importantly--without having to duplicate many (potentially
          # large) strings.
          begin
            # `data` is split after having its last `\n` removed by
            # chomp (if any). This prevents the creation of an empty
            # element after the final `\n` character on POSIX files.
            data.chomp.split(encoded_newlines_re, -1)
          rescue Encoding::ConverterNotFoundError
            # The data is not splittable in the detected encoding.  Assume it's
            # one big line.
            [data]
          end
        else
          []
        end
    end

    def encoded_newlines_re
      @encoded_newlines_re ||= Regexp.union(["\r\n", "\r", "\n"].
                                              map { |nl| nl.encode(ruby_encoding, "ASCII-8BIT").force_encoding(data.encoding) })

    end

    def first_lines(n)
      return lines[0...n] if defined? @lines
      return [] unless viewable? && data

      i, c = 0, 0
      while c < n && j = data.index(encoded_newlines_re, i)
        i = j + $&.length
        c += 1
      end
      data[0...i].split(encoded_newlines_re, -1)
    end

    def last_lines(n)
      if defined? @lines
        if n >= @lines.length
          @lines
        else
          lines[-n..-1]
        end
      end
      return [] unless viewable? && data

      no_eol = true
      i, c = data.length, 0
      k = i
      while c < n && j = data.rindex(encoded_newlines_re, i - 1)
        if c == 0 && j + $&.length == i
          no_eol = false
          n += 1
        end
        i = j
        k = j + $&.length
        c += 1
      end
      r = data[k..-1].split(encoded_newlines_re, -1)
      r.pop if !no_eol
      r
    end

    # Public: Get number of lines of code
    #
    # Requires Blob#data
    #
    # Returns Integer
    def loc
      lines.size
    end

    # Public: Get number of source lines of code
    #
    # Requires Blob#data
    #
    # Returns Integer
    def sloc
      lines.grep(/\S/).size
    end

    # Public: Is the blob a generated file?
    #
    # Generated source code is suppressed in diffs and is ignored by
    # language statistics.
    #
    # May load Blob#data
    #
    # Return true or false
    def generated?
      @_generated ||= Generated.generated?(path, lambda { data })
    end

    # Public: Detects the Language of the blob.
    #
    # May load Blob#data
    #
    # Returns a Language or nil if none is detected
    def language
      @language ||= Linguist.detect(self)
    end

    # Internal: Get the TextMate compatible scope for the blob
    def tm_scope
      language && language.tm_scope
    end

    DETECTABLE_TYPES = [:programming, :markup].freeze

    # Internal: Should this blob be included in repository language statistics?
    def include_in_language_stats?
      !vendored? &&
      !documentation? &&
      !generated? &&
      language && ( defined?(detectable?) && !detectable?.nil? ?
        detectable? :
        DETECTABLE_TYPES.include?(language.type)
      )
    end
  end
end

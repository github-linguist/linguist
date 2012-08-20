require 'linguist/generated'
require 'linguist/language'
require 'linguist/mime'

require 'charlock_holmes'
require 'escape_utils'
require 'pygments'
require 'yaml'

module Linguist
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

    # Public: Get the actual blob mime type
    #
    # Examples
    #
    #   # => 'text/plain'
    #   # => 'text/html'
    #
    # Returns a mime type String.
    def mime_type
      @mime_type ||= Mime.mime_for(extname.to_s)
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
        "attachment; filename=#{EscapeUtils.escape_url(File.basename(name))}"
      end
    end

    def encoding
      if hash = detect_encoding
        hash[:encoding]
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

    # Public: Is the blob binary according to its mime type
    #
    # Return true or false
    def binary_mime_type?
      if mime_type = Mime.lookup_mime_type_for(extname)
        mime_type.binary?
      end
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
      ['.png', '.jpg', '.jpeg', '.gif'].include?(extname)
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
    # We use Pygments.rb for syntax highlighting blobs, which
    # has some quirks and also is essentially 'un-killable' via
    # normal timeout.  To workaround this we try to
    # carefully handling Pygments.rb anything it can't handle.
    #
    # Return true or false
    def safe_to_colorize?
      text? && !large? && !high_ratio_of_long_lines?
    end

    # Internal: Does the blob have a ratio of long lines?
    #
    # These types of files are usually going to make Pygments.rb
    # angry if we try to colorize them.
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
      name =~ VendoredRegexp ? true : false
    end

    # Public: Get each line of data
    #
    # Requires Blob#data
    #
    # Returns an Array of lines
    def lines
      @lines ||= (viewable? && data) ? data.split("\n", -1) : []
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
    # Generated source code is supressed in diffs and is ignored by
    # language statistics.
    #
    # May load Blob#data
    #
    # Return true or false
    def generated?
      @_generated ||= Generated.generated?(name, lambda { data })
    end

    # Public: Should the blob be indexed for searching?
    #
    # Excluded:
    # - Files over 0.1MB
    # - Non-text files
    # - Langauges marked as not searchable
    # - Generated source files
    #
    # Please add additional test coverage to
    # `test/test_blob.rb#test_indexable` if you make any changes.
    #
    # Return true or false
    def indexable?
      if binary?
        false
      elsif extname == '.txt'
        true
      elsif language.nil?
        false
      elsif !language.searchable?
        false
      elsif generated?
        false
      elsif size > 100 * 1024
        false
      else
        true
      end
    end

    # Public: Detects the Language of the blob.
    #
    # May load Blob#data
    #
    # Returns a Language or nil if none is detected
    def language
      return @language if defined? @language
      data_loader = lambda { binary_mime_type? ? "" : data }
      @language = Language.detect(name.to_s, data_loader, mode)
    end

    # Internal: Get the lexer of the blob.
    #
    # Returns a Lexer.
    def lexer
      language ? language.lexer : Pygments::Lexer.find_by_name('Text only')
    end

    # Public: Highlight syntax of blob
    #
    # options - A Hash of options (defaults to {})
    #
    # Returns html String
    def colorize(options = {})
      return unless safe_to_colorize?
      options[:options] ||= {}
      options[:options][:encoding] ||= encoding
      lexer.highlight(data, options)
    end

    # Public: Highlight syntax of blob without the outer highlight div
    # wrapper.
    #
    # options - A Hash of options (defaults to {})
    #
    # Returns html String
    def colorize_without_wrapper(options = {})
      if text = colorize(options)
        text[%r{<div class="highlight"><pre>(.*?)</pre>\s*</div>}m, 1]
      else
        ''
      end
    end
  end
end

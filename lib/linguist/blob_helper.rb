require 'linguist/language'
require 'linguist/mime'
require 'linguist/pathname'

require 'escape_utils'
require 'yaml'

module Linguist
  # BlobHelper is a mixin for Blobish classes that respond to "name",
  # "data" and "size" such as Grit::Blob.
  module BlobHelper
    # Internal: Get a Pathname wrapper for Blob#name
    #
    # Returns a Pathname.
    def pathname
      Pathname.new(name || "")
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
      @mime_type ||= pathname.mime_type
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
      pathname.content_type
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
      case content_type
      when 'application/octet-stream', 'application/java-archive'
        "attachment; filename=#{EscapeUtils.escape_url(pathname.basename)}"
      else
        'inline'
      end
    end

    # Public: Is the blob text?
    #
    # Return true or false
    def text?
      content_type[/(text|json)/]
    end

    # Public: Is the blob a supported image format?
    #
    # Return true or false
    def image?
      ['.png', '.jpg', '.jpeg', '.gif'].include?(pathname.extname)
    end

    # Public: Is the blob binary?
    #
    # Return true or false
    def binary?
      content_type.include?('octet') || !(text? || image?)
    end

    MEGABYTE = 1024 * 1024

    # Public: Is the blob too big to load?
    #
    # Return true or false
    def large?
      size.to_i > MEGABYTE
    end

    # Public: Is the blob viewable?
    #
    # Non-viewable blobs will just show a "View Raw" link
    #
    # Return true or false
    def viewable?
      !image? && !binary? && !large?
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
      name =~ VendoredRegexp
    end

    ################################################################
    # Below here are methods they may access Blob#data. Consider the
    # performance implications of loading it.
    ################################################################

    # Public: Get each line of data
    #
    # Requires Blob#data
    #
    # Returns an Array of lines
    def lines
      @lines ||= data ? data.split("\n", -1) : []
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
    # langauge statistics.
    #
    # Includes:
    # - XCode project XML files
    # - Minified JavaScript
    #
    # Return true or false
    def generated?
      if ['.xib', '.nib', '.pbxproj'].include?(pathname.extname)
        true
      elsif pathname.extname == '.js'
        # JS is minified if any lines are longer than 1000c
        lines.any? { |l| l.length > 1000 }
      else
        false
      end
    end

    # Public: Should the blob be indexed for searching?
    #
    # Excluded:
    # - Non-text files
    # - Generated source files
    # - .po and .sql files
    #
    # Return true or false
    def indexable?
      if !text?
        false
      elsif generated?
        false
      elsif ['.po', '.sql'].include?(pathname.extname)
        false
      elsif Language.find_by_extension(pathname.extname)
        true
      else
        false
      end
    end

    # Public: Determine if the blob contains bad content that can be
    # used for various cross site attacks.
    #
    # Right now this is limited to flash files -- the flash  plugin
    # ignores the response content type and treats any URL as flash
    # when the <object> tag is specified correctly regardless of file
    # extension.
    #
    # Requires Blob#data
    #
    # Returns true when the blob data should not be served with any
    # content-type.
    def forbidden?
      if data = self.data
        data.size >= 8 &&                 # all flash has at least 8 bytes
          %w(CWS FWS).include?(data[0,3]) # file type sigs
      end
    end

    # Public: Detects the Language of the blob.
    #
    # May load Blob#data
    #
    # Returns a Language object
    def language
      if text?
        # First see if there is a Language for the extension
        if Language.find_by_extension(pathname.extname)
          pathname.language

        # Try to detect Language from shebang line
        elsif language = shebang_language
          language

        # Default to Pathname#language
        else
          pathname.language
        end
      else
        Language['Text']
      end
    end

    # Deprecated: Get the lexer of the blob.
    #
    # Returns a Lexer.
    def lexer
      language.lexer
    end

    # Internal: Extract the script name from the shebang line
    #
    # Requires Blob#data
    #
    # Examples
    #
    #   '#!/usr/bin/ruby'
    #   # => 'ruby'
    #
    #   '#!/usr/bin/env ruby'
    #   # => 'ruby'
    #
    #   '#!/usr/bash/python2.4'
    #   # => 'python'
    #
    # Returns a script name String or nil
    def shebang_script
      # Fail fast if blob isn't viewable?
      return unless viewable?

      if data && (match = data.match(/(.+)\n?/)) && (bang = match[0]) =~ /^#!/
        bang.sub!(/^#! /, '#!')
        tokens = bang.split(' ')
        pieces = tokens.first.split('/')
        if pieces.size > 1
          script = pieces.last
        else
          script = pieces.first.sub('#!', '')
        end

        script = script == 'env' ? tokens[1] : script

        # python2.4 => python
        if script =~ /((?:\d+\.?)+)/
          script.sub! $1, ''
        end

        script
      end
    end

    shebangs = YAML.load_file(File.expand_path("../shebangs.yml", __FILE__))
    Shebangs = shebangs.inject({}) { |h, (name, scripts)|
      scripts.each { |script| h[script] = Language[name] }
      h
    }

    # Internal: Get Language for shebang script
    #
    # Matches script name with shebang script name mappings in "shebangs.yml"
    #
    # Returns the Language or nil
    def shebang_language
      if script = shebang_script
        if lang = Shebangs[script]
          lang
        else
          lang = Language.find_by_lexer(script)
          lang != Language['Text'] ? lang : nil
        end
      end
    end

    # Public: Highlight syntax of blob
    #
    # Returns html String
    def colorize
      return if !text? || large?
      lexer.colorize(data)
    end

    # Public: Highlight syntax of blob without the outer highlight div
    # wrapper.
    #
    # Returns html String
    def colorize_without_wrapper
      return if !text? || large?
      lexer.colorize_without_wrapper(data)
    end
  end
end

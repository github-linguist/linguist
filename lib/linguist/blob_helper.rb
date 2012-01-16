require 'linguist/language'
require 'linguist/mime'
require 'linguist/pathname'

require 'charlock_holmes'
require 'escape_utils'
require 'pygments'
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

    # Public: Get the extname of the path
    #
    # Examples
    #
    #   blob(name='foo.rb').extname
    #   # => '.rb'
    #
    # Returns a String
    def extname
      pathname.extname
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
      @content_type ||= binary? ? mime_type :
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
        "attachment; filename=#{EscapeUtils.escape_url(pathname.basename)}"
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
      if mime_type = Mime.lookup_mime_type_for(pathname.extname)
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

    # Public: Is the blob a possible drupal php file?
    #
    # Return true or false
    def drupal_extname?
      ['.module', '.install', '.test', '.inc'].include?(extname)
    end

    # Public: Is the blob likely to have a shebang?
    #
    # Return true or false
    def shebang_extname?
      extname.empty? &&
        mode &&
        (mode.to_i(8) & 05) == 05
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

    # Internal: Compute average line length.
    #
    # Returns Integer.
    def average_line_length
      if lines.any?
        lines.inject(0) { |n, l| n += l.length } / lines.length
      else
        0
      end
    end

    # Public: Is the blob a generated file?
    #
    # Generated source code is supressed in diffs and is ignored by
    # language statistics.
    #
    # Requires Blob#data
    #
    # Includes:
    # - XCode project XML files
    # - Minified JavaScript
    #
    # Please add additional test coverage to
    # `test/test_blob.rb#test_generated` if you make any changes.
    #
    # Return true or false
    def generated?
      if xcode_project_file?
        true
      elsif generated_coffeescript? || minified_javascript? || generated_net_docfile?
        true
      else
        false
      end
    end

    # Internal: Is the blob an XCode project file?
    #
    # Generated if the file extension is an XCode project
    # file extension.
    #
    # Returns true of false.
    def xcode_project_file?
      ['.xib', '.nib', '.pbxproj', '.xcworkspacedata', '.xcuserstate'].include?(extname)
    end

    # Internal: Is the blob minified JS?
    #
    # Consider JS minified if the average line length is
    # greater then 100c.
    #
    # Returns true or false.
    def minified_javascript?
      return unless extname == '.js'
      average_line_length > 100
    end

    # Internal: Is the blob JS generated by CoffeeScript?
    #
    # Requires Blob#data
    #
    # CoffeScript is meant to output JS that would be difficult to
    # tell if it was generated or not. Look for a number of patterns
    # outputed by the CS compiler.
    #
    # Return true or false
    def generated_coffeescript?
      return unless extname == '.js'

      # CoffeeScript generated by > 1.2 include a comment on the first line
      if lines[0] =~ /^\/\/ Generated by /
        return true
      end

      if lines[0] == '(function() {' &&     # First line is module closure opening
          lines[-2] == '}).call(this);' &&  # Second to last line closes module closure
          lines[-1] == ''                   # Last line is blank

        score = 0

        lines.each do |line|
          if line =~ /var /
            # Underscored temp vars are likely to be Coffee
            score += 1 * line.gsub(/(_fn|_i|_len|_ref|_results)/).count

            # bind and extend functions are very Coffee specific
            score += 3 * line.gsub(/(__bind|__extends|__hasProp|__indexOf|__slice)/).count
          end
        end

        # Require a score of 3. This is fairly arbitrary. Consider
        # tweaking later.
        score >= 3
      else
        false
      end
    end

    # Internal: Is this a generated documentation file for a .NET assembly?
    #
    # Requires Blob#data
    #
    # .NET developers often check in the XML Intellisense file along with an
    # assembly - however, these don't have a special extension, so we have to
    # dig into the contents to determine if it's a docfile. Luckily, these files
    # are extremely structured, so recognizing them is easy.
    #
    # Returns true or false
    def generated_net_docfile?
      return false unless extname.downcase == ".xml"
      return false unless lines.count > 3

      # .NET Docfiles always open with <doc> and their first tag is an
      # <assembly> tag
      return lines[1].include?("<doc>") &&
        lines[2].include?("<assembly>") &&
        lines[-2].include?("</doc>")
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
      if defined? @language
        @language
      else
        @language = guess_language
      end
    end

    # Internal: Guess language
    #
    # Please add additional test coverage to
    # `test/test_blob.rb#test_language` if you make any changes.
    #
    # Returns a Language or nil
    def guess_language
      return if binary_mime_type?

      # Disambiguate between multiple language extensions
      disambiguate_extension_language ||

        # See if there is a Language for the extension
        pathname.language ||

        # Look for idioms in first line
        first_line_language ||

        # Try to detect Language from shebang line
        shebang_language
    end

    # Internal: Get the lexer of the blob.
    #
    # Returns a Lexer.
    def lexer
      language ? language.lexer : Pygments::Lexer.find_by_name('Text only')
    end

    # Internal: Disambiguates between multiple language extensions.
    #
    # Delegates to "guess_EXTENSION_language".
    #
    # Please add additional test coverage to
    # `test/test_blob.rb#test_language` if you add another method.
    #
    # Returns a Language or nil.
    def disambiguate_extension_language
      if Language.ambiguous?(extname)
        name = "guess_#{extname.sub(/^\./, '')}_language"
        send(name) if respond_to?(name)
      end
    end

    # Internal: Guess language of header files (.h).
    #
    # Returns a Language.
    def guess_h_language
      if lines.grep(/^@(interface|property|private|public|end)/).any?
        Language['Objective-C']
      elsif lines.grep(/^class |^\s+(public|protected|private):/).any?
        Language['C++']
      else
        Language['C']
      end
    end

    # Internal: Guess language of .m files.
    #
    # Objective-C heuristics:
    # * Keywords
    #
    # Matlab heuristics:
    # * Leading function keyword
    # * "%" comments
    #
    # Returns a Language.
    def guess_m_language
      # Objective-C keywords
      if lines.grep(/^#import|@(interface|implementation|property|synthesize|end)/).any?
        Language['Objective-C']

      # File function
      elsif lines.first.to_s =~ /^function /
        Language['Matlab']

      # Matlab comment
      elsif lines.grep(/^%/).any?
        Language['Matlab']

      # Fallback to Objective-C, don't want any Matlab false positives
      else
        Language['Objective-C']
      end
    end

    # Internal: Guess language of .pl files
    #
    # The rules for disambiguation are:
    #
    # 1. Many perl files begin with a shebang
    # 2. Most Prolog source files have a rule somewhere (marked by the :- operator)
    # 3. Default to Perl, because it is more popular
    #
    # Returns a Language.
    def guess_pl_language
      if shebang_script == 'perl'
        Language['Perl']
      elsif lines.grep(/:-/).any?
        Language['Prolog']
      else
        Language['Perl']
      end
    end

    # Internal: Guess language of .r files.
    #
    # Returns a Language.
    def guess_r_language
      if lines.grep(/(rebol|(:\s+func|make\s+object!|^\s*context)\s*\[)/i).any?
        Language['Rebol']
      else
        Language['R']
      end
    end

    # Internal: Guess language of .t files.
    #
    # Returns a Language.
    def guess_t_language
      score = 0
      score += 1 if lines.grep(/^% /).any?
      score += data.gsub(/ := /).count
      score += data.gsub(/proc |procedure |fcn |function /).count
      score += data.gsub(/var \w+: \w+/).count

      # Tell-tale signs its gotta be Perl
      if lines.grep(/^(my )?(sub |\$|@|%)\w+/).any?
        score = 0
      end

      if score >= 3
        Language['Turing']
      else
        Language['Perl']
      end
    end

    # Internal: Guess language of .gsp files.
    #
    # Returns a Language.
    def guess_gsp_language
      if lines.grep(/<%|<%@|\$\{|<%|<g:|<meta name="layout"|<r:/).any?
        Language['Groovy Server Pages']
      else
        Language['Gosu']
      end
    end

    # Internal: Guess language from the first line.
    #
    # Look for leading "<?php" in Drupal files
    #
    # Returns a Language.
    def first_line_language
      # Only check files with drupal php extensions
      return unless drupal_extname?

      # Fail fast if blob isn't viewable?
      return unless viewable?

      if lines.first.to_s =~ /^<\?php/
        Language['PHP']
      end
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
    # Please add additional test coverage to
    # `test/test_blob.rb#test_shebang_script` if you make any changes.
    #
    # Returns a script name String or nil
    def shebang_script
      # Fail fast if blob isn't viewable?
      return unless viewable?

      if lines.any? && (match = lines[0].match(/(.+)\n?/)) && (bang = match[0]) =~ /^#!/
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

        # Check for multiline shebang hacks that exec themselves
        #
        #   #!/bin/sh
        #   exec foo "$0" "$@"
        #
        if script == 'sh' &&
            lines[0...5].any? { |l| l.match(/exec (\w+).+\$0.+\$@/) }
          script = $1
        end

        script
      end
    end

    # Internal: Get Language for shebang script
    #
    # Returns the Language or nil
    def shebang_language
      # Skip file extensions unlikely to have shebangs
      return unless shebang_extname?

      if script = shebang_script
        Language[script]
      end
    end

    # Public: Highlight syntax of blob
    #
    # options - A Hash of options (defaults to {})
    #
    # Returns html String
    def colorize(options = {})
      return if !text? || large? || generated?
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

    Language.overridden_extensions.each do |extension|
      name = "guess_#{extension.sub(/^\./, '')}_language".to_sym
      unless instance_methods.map(&:to_sym).include?(name)
        warn "Language##{name} was not defined"
      end
    end
  end
end

require 'linguist/language'
require 'linguist/mime'
require 'linguist/pathname'

require 'escape_utils'

module Linguist
  module BlobHelper
    def pathname
      Pathname.new(name || "")
    end

    def mime_type
      @mime_type ||= pathname.mime_type
    end

    def content_type
      pathname.content_type
    end

    def disposition
      case content_type
      when 'application/octet-stream', 'application/java-archive'
        "attachment; filename=#{EscapeUtils.escape_url(pathname.basename)}"
      else
        'inline'
      end
    end

    def lines
      @lines ||= data ? data.split("\n", -1) : []
    end

    def loc
      lines.size
    end

    def sloc
      lines.grep(/\S/).size
    end

    def binary?
      content_type.include?('octet') || !(text? || image?)
    end

    def file?
      image? || binary?
    end

    def text?
      content_type[/(text|json)/]
    end

    def image?
      ['.png', '.jpg', '.jpeg', '.gif'].include?(pathname.extname)
    end

    MEGABYTE = 1024 * 1024

    def large?
      size.to_i > MEGABYTE
    end

    def viewable?
      !file? && !large?
    end

    def generated?
      ['.xib', '.nib', '.pbxproj'].include?(pathname.extname)
    end

    def language
      if text?
        shebang_language || pathname.language
      else
        Language['Text']
      end
    end

    def lexer_name
      language.lexer_name
    end

    def lexer
      language.lexer
    end

    def shebang_script
      return if !text? || large?

      if (match = data.match(/(.+)\n?/)) && (bang = match[0]) =~ /^#!/
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

    def shebang_language
      if script = shebang_script
        case script
        when 'bash'
          Language['Shell']
        when 'groovy'
          Language['Java']
        when 'macruby'
          Language['Ruby']
        when 'node'
          Language['JavaScript']
        when 'rake'
          Language['Ruby']
        when 'sh'
          Language['Shell']
        when 'zsh'
          Language['Shell']
        else
          lang = Language.find_by_lexer(shebang_script)
          lang != Language['Text'] ? lang : nil
        end
      end
    end

    def colorize
      return if !text? || large?
      lexer.colorize(data)
    end

    def colorize_without_wrapper
      return if !text? || large?
      lexer.colorize_without_wrapper(data)
    end
  end
end

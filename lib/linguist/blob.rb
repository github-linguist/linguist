require 'linguist/language'
require 'linguist/mime'
require 'linguist/pathname'

require 'escape_utils'

module Linguist
  class Blob
    def initialize(blob)
      @blob = blob
    end

    def name
      @blob.name
    end

    def pathname
      Pathname.new(name || "")
    end

    def mime_type
      Mime.lookup(pathname.extname)
    end

    def disposition
      case mime_type
      when 'application/octet-stream', 'application/java-archive'
        "attachment; filename=#{EscapeUtils.escape_url(pathname.basename)}"
      else
        'inline'
      end
    end

    def data
      @blob.data
    end

    def lines
      @lines ||= data ? data.split("\n", -1) : []
    end

    def size
      @blob.size
    end

    def loc
      lines.size
    end

    def sloc
      lines.grep(/\S/).size
    end

    def submodule?
      defined?(Grit::Submodule) && @blob.kind_of?(Grit::Submodule)
    end

    def binary?
      mime_type == 'octet-stream' || !(text? || image?)
    end

    def file?
      image? || binary?
    end

    def text?
      return false if submodule?

      pathname.media_type == 'text' ||
        pathname.mime_type == 'application/json'
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

    def language
      if text?
        shebang_language || pathname.language
      else
        Language['Text']
      end
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
  end
end

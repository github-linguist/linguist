module ReverseMarkdown
  module Converters
    class H < Base
      def convert(node, state = {})
        prefix = '#' * node.name[/\d/].to_i
        content = treat_children(node, state).strip
        # Merge lines into one (markdown headings can't span multiple lines)
        content = content.split(/\s*\n\s*/).join(' ')
        "\n#{prefix} #{content}\n"
      end
    end

    register :h1, H.new
    register :h2, H.new
    register :h3, H.new
    register :h4, H.new
    register :h5, H.new
    register :h6, H.new
  end
end

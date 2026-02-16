module ReverseMarkdown
  module Converters
    class Base
      def treat_children(node, state)
        node.children.inject(+'') do |memo, child|
          memo << treat(child, state)
        end
      end

      def treat(node, state)
        ReverseMarkdown::Converters.lookup(node.name).convert(node, state)
      end

      def escape_keychars(string)
        string.gsub(/(?<!\\)[*_]/, '*' => '\*', '_' => '\_')
      end

      # Wrap content with markers (e.g., ** or _), splitting at paragraph breaks
      # so markers don't span across breaks (which breaks markdown rendering)
      def wrap_with_markers(content, marker)
        # Split on paragraph breaks, preserving the breaks
        segments = content.split(/(\s*\n\s*\n\s*)/)

        segments.map.with_index do |segment, i|
          if i.odd?  # This is a break segment (captured delimiter)
            segment
          elsif segment.strip.empty?
            segment
          else
            # Wrap with markers, preserving border whitespace
            leading = segment[/\A\s*/]
            trailing = segment[/\s*\z/]
            "#{leading}#{marker}#{segment.strip}#{marker}#{trailing}"
          end
        end.join
      end

      def extract_title(node)
        title = escape_keychars(node['title'].to_s)
        title.empty? ? '' : %[ "#{title}"]
      end

      def extract_src(node)
        node['src'].to_s.empty? ? '' : node['src'].to_s
      end
    end
  end
end

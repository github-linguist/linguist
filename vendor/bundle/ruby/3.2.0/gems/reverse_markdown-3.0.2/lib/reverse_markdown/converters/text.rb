module ReverseMarkdown
  module Converters
    class Text < Base
      def convert(node, options = {})
        if node.text.strip.empty?
          treat_empty(node)
        else
          treat_text(node)
        end
      end

      private

      INLINE_ELEMENTS = [:a, :abbr, :b, :bdi, :bdo, :cite, :code, :data, :del,
                          :dfn, :em, :i, :ins, :kbd, :mark, :q, :rp, :rt, :ruby,
                          :s, :samp, :small, :span, :strong, :sub, :sup, :time,
                          :u, :var, :wbr, :font, :tt].freeze

      def treat_empty(node)
        parent = node.parent.name.to_sym
        if [:ol, :ul].include?(parent)  # Otherwise the identation is broken
          ''
        elsif node.text == ' '          # Regular whitespace text node
          ' '
        elsif INLINE_ELEMENTS.include?(parent) && node.text =~ /\n/
          # Preserve newlines between inline elements as space (HTML whitespace collapsing)
          ' '
        else
          ''
        end
      end

      def treat_text(node)
        text = node.text
        text = preserve_nbsp(text)
        text = remove_border_newlines(text, node)
        text = remove_inner_newlines(text)
        text = escape_keychars(text)

        text = preserve_keychars_within_backticks(text)
        text = preserve_tags(text)

        text
      end

      def preserve_nbsp(text)
        text.gsub(/\u00A0/, "&nbsp;")
      end

      def preserve_tags(text)
        text.gsub(/[<>]/, '>' => '\>', '<' => '\<')
      end

      def remove_border_newlines(text, node)
        # Convert leading newlines to space if there's preceding inline content
        result = if has_adjacent_inline_content?(node, :previous)
          text.gsub(/\A\n+/, ' ')
        else
          text.gsub(/\A\n+/, '')
        end

        # Convert trailing newlines to space if there's following inline content
        if has_adjacent_inline_content?(node, :next)
          result.gsub(/\n+\z/, ' ')
        else
          result.gsub(/\n+\z/, '')
        end
      end

      def has_adjacent_inline_content?(node, direction)
        sibling = direction == :next ? node.next_sibling : node.previous_sibling
        while sibling
          if sibling.text?
            return true unless sibling.text.strip.empty?
          elsif INLINE_ELEMENTS.include?(sibling.name.to_sym)
            return true
          else
            return false
          end
          sibling = direction == :next ? sibling.next_sibling : sibling.previous_sibling
        end

        parent = node.parent
        return false unless INLINE_ELEMENTS.include?(parent.name.to_sym)

        has_adjacent_inline_content?(parent, direction)
      end

      def remove_inner_newlines(text)
        text.tr("\r\n\t", ' ').squeeze(' ')
      end

      def preserve_keychars_within_backticks(text)
        text.gsub(/`.*?`/) do |match|
          match.gsub('\_', '_').gsub('\*', '*')
        end
      end
    end

    register :text, Text.new
  end
end

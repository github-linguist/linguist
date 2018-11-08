module Linguist
  module Strategy
    # Detect a well-formed man(7) or mdoc(7) manpage
    class Manpage

      # Number of lines to search at the beginning of the file
      #
      # Ample room should be given to accommodate authors who
      # embed hand-rolled macros or licensing info at the start
      # of their documents.
      SEARCH_SCOPE = 500

      # Public: RegExp for matching conventional manpage extensions
      #
      # This is the same expression as that used by `github/markup`
      MANPAGE_EXTS = /\.(?:[1-9](?![0-9])[a-z_0-9]*|0p|n|man|mdoc)(?:\.in)?$/i


      # Public: Detect a Roff manpage based on its content and file extension.
      #
      # blob       - An object that quacks like a blob.
      # candidates - A list of candidate languages
      #
      # Returns the candidates array if it wasn't empty, [Language["Roff"]]
      # if the strategy matched, and an empty array if the strategy didn't. 
      def self.call(blob, candidates = [])
        return candidates if candidates.any?
        Manpage.new(blob).well_formed? ? [Language["Roff"]] : []
      end

      def initialize(blob)
        @blob = blob
        @valid_prologue = nil
        @well_formed = nil
      end

      # Is this a Roff manpage that begins with a recognisable prologue? 
      def well_formed?
        @well_formed ||= valid_ext? && valid_prologue?
      end

      # Is the file extension a conventional-looking manpage suffix?
      def valid_ext?
        if @blob.name =~ MANPAGE_EXTS
          @name = $`
          @ext = $~
          true
        elsif
          false
        end
      end

      # Scan the document's header in search of a .Dt or .TH line
      def valid_prologue?
        return unless @valid_prologue.nil?

        # A single-line manpage could only be valid if it's a `.so` redirect
        if @blob.sloc < 2
          @valid_prologue = so_request?(@blob.lines[0])

        # Short-circuit on pages which have a preprocessor hint as their first line:
        # https://www.gnu.org/software/groff/manual/html_node/Preprocessors-in-man-pages.html
        elsif /^'\\" [tre]+(?=\s|$)/.match?(@blob.lines[0])
          @valid_prologue = true

        # Scan until a title declaration is reached, or an input line is found.
        # Well-formed manpages will never have content before their prologues.
        else
          @blob.first_lines(SEARCH_SCOPE).each do |line|
            if empty_or_comment_only?(line)
              next
            # We've found the title before the first visible line of text
            elsif title_declaration?(line)
              @valid_prologue = true
              break
            # Skip unrelated Roff commands, keep searching for a Dt/TH macro
            elsif command_line?(line)
              next
            # There shouldn't be content before the document title
            else
              @valid_prologue = false
              break
            end
          end
          @valid_prologue
        end
      end

      def empty_or_comment_only?(line)
        /^[.']?[ \t]*(?=$|\\")/.match?(line)
      end

      def so_request?(line)
        /^[.'][ \t]*so[ \t]+\S/.match?(line)
      end

      def title_declaration?(line)
        if /^[.'][ \t]*(Dt|TH)[ \t]+(\S.*)$/.match(line)
          @macro_name = $1
          @macro_args = $2

          # Set "flavour" by macro type
          @uses_mdoc = false
          @uses_man = false
          case @macro_name
            when "Dt"; @uses_mdoc = true
            when "TH"; @uses_man  = true
          end
        end
      end

      def command_line?(line)
        /^[.'][ \t]*
        (AT|B|BI|BR|BT|DT|EE|EX|HP|IB|IP|IR|LP|ME|MT|OP|P|PD|PP|PT|R|RB|RE|RI|RS|SB|SH|SM
        |SS|TH|TP|UC|UE|UR|%A|%B|%C|%D|%I|%J|%N|%O|%P|%Q|%R|%T|%U|%V|Ac|Ad|An|Ao|Ap|Aq|Ar
        |At|Bc|Bd|Bf|Bk|Bl|Bo|Bq|Brc|Bro|Brq|Bsx|Bt|Bx|Cd|Cm|D1|Dc|Dd|Dl|Do|Dq|Dt|Dv|Dx|Ec
        |Ed|Ef|Ek|El|Em|En|Eo|Er|Es|Ev|Ex|Fa|Fc|Fd|Fl|Fn|Fo|Fr|Ft|Fx|Hf|Ic|In|It|Lb|Li|Lk
        |Lp|Ms|Mt|Nd|Nm|No|Ns|Nx|Oc|Oo|Op|Os|Ot|Ox|Pa|Pc|Pf|Po|Pp|Pq|Qc|Ql|Qo|Qq|Re|Rs|Rv
        |Sc|Sh|Sm|So|Sq|Ss|St|Sx|Sy|Ta|Tn|Ud|Ux|Va|Vt|Xc|Xo|Xr|ab|ad|af|am|as|bd|bp|br|c2
        |cc|ce|cf|ch|cs|cu|da|de|di|ds|dt|ec|el|em|eo|ev|ex|fc|fi|fl|fp|ft|hc|hw|hy|ie|if
        |ig|in|it|lc|lg|lf|ll|ls|lt|mc|mk|na|ne|nf|nh|nm|nn|nr|ns|nx|os|pc|pi|pl|pm|pn|po
        |ps|rd|rm|rn|rr|rs|rt|so|sp|ss|sv|sy|ta|tc|ti|tl|tm|tr|uf|ul|vs|wh)
        (?=\s|$)/x.match?(line)
      end
    end
  end
end

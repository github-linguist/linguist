require_relative "./grammar_source"
require_relative "./all"

# Public: Represents a registered Git submodule in use by Linguist.
#
# Any updates to this class should consider submodules which aren't
# grammar-related, such as CodeMirror. See also: GrammarSource
#
# Examples
#
#   Submodule.new('vendor/CodeMirror', {url: "codemirror/CodeMirror"})
#   # => #<Submodule url="https://github.com/codemirror/CodeMirror.git">
#
#   Submodule.for_grammar('vendor/grammars/language-roff')
#   # => #<Submodule url="https://github.com/Alhadis/language-roff.git">
#
class Submodule
  attr_accessor :id, :attr

  def initialize(id, attr = {})
    @id = id
    @attr = attr
    @attr[:path] ||= @id

    # If a grammar submodule, store a pointer to source
    if /^vendor\/grammars/.test attr[:url]
      @grammar = GrammarSource.by_url attr[:url]
    end
  end

  def <=>(other)
    @id <=> other.id
  end

  # Is the submodule registered with Git and checked out locally?
  def registered?
    @configured? and @exists?
  end

  # Is the submodule registered with Git?
  def configured?
    system "git", "config", "submodule.#{@id}.url"
  end

  # Has the submodule been checked out locally?
  def exists?
    exists?(@id)
  end

  # Format an entry to use in `.gitmodules`
  def to_s
    attr = @attr.to_a.map do |key, value|
      "\t#{key} = #{value}"
    end
    <<~EOS
    [submodule "#{@id}"]
    #{ attr.sort.join "\n" }
    EOS
  end

  # Define a GrammarSource for an existing registered submodule.
  #
  # path - path of submodule as used by .gitmodules
  def self.for_grammar(path)
    path =~ /^(?:.*(?:vendor\/)?grammars\/)?([^\/]+)/i
    path = "vendor/grammars/#{$1}"
    unless exists?(path)
      raise "Submodule '#{path}' does not exist"
    end
    self.list.by_id[:path]
  end

  # Load the contents of .gitmodules
  def self.list
    if @list.nil?
      all = []
      ids = {}
      pattern = /^\[submodule\s*"([^"]+)"\]$\n((?:^(?!\[).+(?:\n|$))+)/is
      read_file(".gitmodules").scan(pattern) do |id, data|
        attr = {}
        data.match(/^\s*(?<key>[^\s=]+)\s*=\s*(?<value>.+)$/m) do |match|
          attr[match[:key]] = match[:value].strip
        end
        all << ids[id] = self.new(id, attr)
      end
      @list = {all: all.sort, by_id: ids}
    end
    @list
  end
end

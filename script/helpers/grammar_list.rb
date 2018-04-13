require_relative "./grammar_source"
require_relative "./submodule"
require_relative "./helpers"
require "bundler/setup"
require "linguist"
require "json"
require "yaml"

class GrammarList

  ROOT = File.expand_path "../../../", __FILE__

  def initialize
    @submodules     = Submodule.list
    @language_names = load_languages()
    @sources        = load_sources()
  end

  # Grab the name of each language, sorted case-insensitively
  def load_languages
    Linguist::Language.all.map(&:name).sort do |a, b|
      a.downcase() <=> b.downcase()
    end
  end

  # Load grammars.yml
  def load_sources
    sources = {}
    YAML.load_file("#{ROOT}/grammars.yml").each do |path, scopes|
      scopes.each { |scope| sources[scope] = @submodules[path] }
    end
    sources
  end

  # Format list as Markdown
  def to_markdown
    markdown = ""
    @language_names.each do |item|
      lang  = Linguist::Language["#{item}"]
      scope = lang.tm_scope
      next if scope == "none"
      path  = @sources[scope] || scope
      case path
      when "https://bitbucket.org/Clams/sublimesystemverilog/get/default.tar.gz"
        short_url = "bitbucket:Clams/sublimesystemverilog"
        long_url  = "https://bitbucket.org/Clams/sublimesystemverilog"
      when "https://svn.edgewall.org/repos/genshi/contrib/textmate/Genshi.tmbundle/Syntaxes/Markup%20Template%20%28XML%29.tmLanguage"
        short_url = "genshi.edgewall.org/query"
        long_url  = "https://genshi.edgewall.org/query"
      when "vendor/grammars/oz-tmbundle/Syntaxes/Oz.tmLanguage"
        short_url = "eregon/oz-tmbundle"
        long_url  = "https://github.com/eregon/oz-tmbundle"
      else
        submodule = @submodules[@sources[scope].chomp("/")]
        next unless submodule
        short_url = submodule[:short]
        long_url  = submodule[:url]
      end
      markdown += "- **#{item}:** [#{short_url}](#{long_url})\n"
    end
    markdown
  end

  def update_lists
    # Update .gitsubmodules
    sorted = @sources.sort { |a,b| a[0] <=> b[0] }.collect{ |i| i[1] }
    File.write "#{ROOT}/.gitmodules", sorted
    
    # Update the file displaying the reader-friendly list of grammar repos
    readme = "#{ROOT}/vendor/README.md"
    preamble = File.read(readme).match(/\A.+?<!--.+?-->\n/ms)
    list = self.to_markdown
    File.write(readme, preamble.to_s + list)
  end
end

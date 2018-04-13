require_relative "./all"
require_relative "./host"
require_relative "./unique"

# Represents the source of a language grammar
#
# NOTE: Sources are mostly - but not always - connected to a
#       Submodule. Some ad-hoc exceptions exist which aren't
#       connected with a Git repository.
#
class GrammarSource < Unique

  # RegExp for matching trusted domain hosts
  HOSTS = Regexp.union(Host.whitelist)

  def initialize(attr = {})
    @name      = attr[:name]      || nil # Unique name of repository
    @host      = attr[:host]      || nil # Hostname of repo's provider
    @author    = attr[:author]    || nil # Username of repo's author
    @url       = attr[:url]       || nil # Resolved absolute URL

    # Resolve missing properties
    @url       ||= "https://#{@host.long}/#{@author}/#{@name}.git"
    @short_url ||= @host.prefix + @author + "/#{@name}"
    @long_url  ||= @url
  end

  # Format source as a Markdown link
  def to_markdown
    "[#{self.url.short}](#{self.url.long})"
  end

  # Define a grammar source by its upstream URL.
  #
  # url - an HTTPS, HTTP, or SSH address accepted by git-remote(1)
  #       Only domains listed in HOSTS are accepted; unrecognised
  #       hostnames or invalid URLs will raise an ArgumentError.
  #
  #       Assumption: Repo URLs will never include subdomains.
  #       We only check for a possible `www`, nothing else.
  def self.by_url(url)
    case url
    when "https://bitbucket.org/Clams/sublimesystemverilog/get/default.tar.gz"
      self.define({
        name:      "sublimesystemverilog",
        host:      Host.define("bitbucket.org"),
        author:    "Clams",
        url:       url,
        short_url: "bitbucket:Clams/sublimesystemverilog",
        long_url:  "https://bitbucket.org/Clams/sublimesystemverilog"
      })
    when "https://svn.edgewall.org/repos/genshi/contrib/textmate/Genshi.tmbundle/Syntaxes/Markup%20Template%20%28XML%29.tmLanguage"
      self.define({
        name:      "Genshi.tmbundle",
        host:      Host.define("genshi.edgewall.org"),
        url:       url,
        short_url: "genshi.edgewall.org/query",
        long_url:  "https://genshi.edgewall.org/query"
      })
    when "vendor/grammars/oz-tmbundle/Syntaxes/Oz.tmLanguage"
      self.define({
        name:      "oz-tmbundle",
        host:      Host.define("github.com"),
        author:    "eregon",
        url:       url,
        short_url: "eregon/oz-tmbundle",
        long_url:  "https://github.com/eregon/oz-tmbundle"
      })
    else
      if parsed = URL.parse(url)
        self.define(parsed)
      else
        raise ArgumentError, "Unsupported URL: #{url}"
      end
    end
  end

  # Define a new GrammarSource, or reference an existing one
  def self.define(attr)
    unless attr[:url]
      host       = Host.define(attr[:host])
      author     = attr[:author]
      name       = attr[:name]
      attr[:url] = "https://#{host.long}/#{author}/#{name}.git"
    end
    BY_URL[attr[:url]] ||= self.new(attr)
  end
end

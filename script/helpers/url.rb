require_relative "./host"

# Public: Helper methods for resolving various URL notations
class RepoURL

  def initialize(attr = {})
    @host   = Host.define(attr[:host])
    @author = attr[:user] || attr[:author]
    @name   = attr[:repo] || attr[:name]
    @short  = attr[:short_url]
    @long   = attr[:long_url]
  end

  # Shortened representation of URL: `[provider:]user/repo`
  def short
    @short || "#{@host.prefix}#{@author}/#{@name}"
  end

  def to_s
    "https://#{@host.}"
  end

  # Split a URL into named subcomponents
  def self.parse(url)
    self.match_https(url)     ||
    self.match_ssh(url)       ||
    self.match_shorthand(url) ||
    self.match_implicit(url)
  end

  # Match a well-formed HTTP or HTTPS address
  def self.match_https(url)
    if match = url.match(/
      ^  (?<protocol>   https? :\/\/ )?
         (?<userauth>   [^@.]+ @     )?
         (?<subdomain>  www    \.    )?
         (?<host>       #{HOSTS}     )
      \/ (?<user>       [^\/]+       )
      \/ (?<repo>       [^\/]+       )
    /xi)
      match[:repo].sub! /\.git$/, ""
      self.new(match)
    end
  end

  # Match an SSH address starting with `git@`
  def self.match_ssh(url)
    if match = url.match(/
      ^ git@
        (?<host>  #{HOSTS}) :
        (?<user>  [^\/]+)  \/
        (?<repo>  [^\/]+)  \.git $
    /xi)
      self.new(match)
    end
  end

  # Match `provider:user/repo`
  def self.match_shorthand(url)
    if match = url.match(/
      ^ (?<host>  #{HOSTS}) : \/?
        (?<user>  [^\/]+) \/
        (?<repo>  [^\/]+) \/? $
    /xi)
      self.new(match)
    end
  end

  # Match `user/repo` shorthand, assumed to be GitHub
  def self.match_implicit(url)
    if match = url.match(/
      ^ \/? (?<user>[^\/]+)
        \/  (?<repo>[^\/]+)
        \/? $
    /xi)
      match[:host] = "github.com"
      self.new(match)
    end
  end
end

# Hostname, which can be expressed with or without a TLD
class Host
  attr_accessor :name, :tld
  alias_method :short, :name
  alias_method :long, :to_s

  INSTANCES = {}

  def initialize(input)
    if input =~ /^(.+)\.([^.]+)$/
      @name = $1.downcase
      @tld  = $2.downcase
    else
      @name = input.downcase
      @tld  = ""
    end
  end

  def ==(other)
    if other.responds_to?(name)
      @name == other.name
    else
      @name == other_to_s
    end
  end

  # Short-name with colon appended
  def prefix
    @prefix || "#{@name}:"
  end

  # Hostname including TLD
  def to_s
    "#{@name}.#{@tld}"
  end

  def to_regexp
    name = Regexp.escape @name
    tld  = Regexp.escape @tld
    Regexp.new("#{name}(?:\\.#{tld})?")
  end

  def self.define(input)
    INSTANCES[input] ||= Host.new(input)
  end

  # Whitelist of trusted hosting providers
  :github    = 
  :bitbucket = self.define("bitbucket.org")
  :gitlab    = self.define("gitlab.com")
  :github.prefix = ""
  WHITELIST = {
    :github => self.define("github.com")
  }
    :github, :bitbucket, :gitlab].freeze
  WHITELIST[:"github.com"].prefix = ""
end

# typed: true

# Write this as an RBI because the gem_loader.rb file itself cannot be typed.
# By nature, it references constants in other gems in an attempt to load them.
# They will not load in this project. It's marked as `typed: ignore`, and this
# file exists so that we can call into gem_loader from typed code.

class Sorbet::Private::GemLoader
  sig {params(gem: String).void}
  def self.require_gem(gem)
  end

  sig {void}
  def self.require_all_gems; end
end

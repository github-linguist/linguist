# -*- encoding: utf-8 -*-
# stub: rugged 1.9.0 ruby lib
# stub: ext/rugged/extconf.rb

Gem::Specification.new do |s|
  s.name = "rugged".freeze
  s.version = "1.9.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "msys2_mingw_dependencies" => "libssh2 cmake" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Scott Chacon".freeze, "Vicent Marti".freeze]
  s.date = "2025-01-03"
  s.description = "Rugged is a Ruby bindings to the libgit2 linkable C Git library. This is\nfor testing and using the libgit2 library in a language that is awesome.\n".freeze
  s.email = "schacon@gmail.com".freeze
  s.extensions = ["ext/rugged/extconf.rb".freeze]
  s.files = ["ext/rugged/extconf.rb".freeze]
  s.homepage = "https://github.com/libgit2/rugged".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 1.9.3".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Rugged is a Ruby binding to the libgit2 linkable library".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_development_dependency(%q<rake-compiler>.freeze, [">= 0.9.0"])
  s.add_development_dependency(%q<pry>.freeze, [">= 0"])
  s.add_development_dependency(%q<minitest>.freeze, ["~> 5.0"])
end

# -*- encoding: utf-8 -*-
# stub: yajl-ruby 1.4.3 ruby lib
# stub: ext/yajl/extconf.rb

Gem::Specification.new do |s|
  s.name = "yajl-ruby".freeze
  s.version = "1.4.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Brian Lopez".freeze, "Lloyd Hilaiel".freeze]
  s.date = "2022-05-26"
  s.email = "seniorlopez@gmail.com".freeze
  s.extensions = ["ext/yajl/extconf.rb".freeze]
  s.files = ["ext/yajl/extconf.rb".freeze]
  s.homepage = "https://github.com/brianmario/yajl-ruby".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Ruby C bindings to the excellent Yajl JSON stream-based parser library.".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_development_dependency(%q<rake-compiler>.freeze, [">= 0"])
  s.add_development_dependency(%q<rspec>.freeze, [">= 0"])
  s.add_development_dependency(%q<activesupport>.freeze, [">= 0"])
  s.add_development_dependency(%q<json>.freeze, [">= 0"])
  s.add_development_dependency(%q<benchmark-memory>.freeze, [">= 0"])
end

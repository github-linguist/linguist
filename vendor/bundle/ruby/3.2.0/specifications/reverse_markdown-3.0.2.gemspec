# -*- encoding: utf-8 -*-
# stub: reverse_markdown 3.0.2 ruby lib

Gem::Specification.new do |s|
  s.name = "reverse_markdown".freeze
  s.version = "3.0.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Johannes Opper".freeze]
  s.date = "2026-01-20"
  s.description = "Map simple html back into markdown, e.g. if you want to import existing html data in your application.".freeze
  s.email = ["johannes.opper@gmail.com".freeze]
  s.executables = ["reverse_markdown".freeze]
  s.files = ["bin/reverse_markdown".freeze]
  s.homepage = "http://github.com/xijo/reverse_markdown".freeze
  s.licenses = ["WTFPL".freeze]
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Convert html code into markdown.".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<nokogiri>.freeze, [">= 0"])
  s.add_development_dependency(%q<rspec>.freeze, [">= 0"])
  s.add_development_dependency(%q<simplecov>.freeze, [">= 0"])
  s.add_development_dependency(%q<rake>.freeze, [">= 0"])
  s.add_development_dependency(%q<kramdown>.freeze, [">= 0"])
  s.add_development_dependency(%q<debug>.freeze, [">= 0"])
end

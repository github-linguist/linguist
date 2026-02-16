Gem::Specification.new do |gem|
  gem.name          = 'pathname-common_prefix'
  gem.version       = '0.0.2'
  gem.authors       = ['KITAITI Makoto']
  gem.email         = ['KitaitiMakoto@gmail.com']
  gem.description   = 'This file provides `Pathname.common_prefix` and `Pathname#common_prefix` which calculate the common prefix in the passed paths.'
  gem.summary       = 'Calculate prefix commont to some pathnames'
  gem.homepage      = 'https://gitlab.com/KitaitiMakoto/pathname-common_prefix'

  gem.metadata["source_code_uri"] = "https://gitlab.com/KitaitiMakoto/pathname-common_prefix"

  gem.files         = %w[
                         lib/pathname/common_prefix.rb
                         test/test_common_prefix.rb
                         bin/common-prefix
                         README.markdown
                         Rakefile setup.rb
                         pathname-common_prefix.gemspec
                         Steepfile sig/pathname-common_prefix.rbs
                      ]
  gem.executables   = %w[common-prefix]
  gem.test_files    = %w[test/test_common_prefix.rb]
  gem.require_paths = %w[lib]
end

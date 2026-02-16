# frozen_string_literal: true
module Licensed
  module Sources
    require "licensed/sources/source"
    require "licensed/sources/bower"
    require "licensed/sources/bundler"
    require "licensed/sources/cabal"
    require "licensed/sources/cargo"
    require "licensed/sources/cocoapods"
    require "licensed/sources/composer"
    require "licensed/sources/dep"
    require "licensed/sources/git_submodule"
    require "licensed/sources/go"
    require "licensed/sources/gradle"
    require "licensed/sources/manifest"
    require "licensed/sources/mix"
    require "licensed/sources/npm"
    require "licensed/sources/nuget"
    require "licensed/sources/pip"
    require "licensed/sources/pipenv"
    require "licensed/sources/pnpm"
    require "licensed/sources/swift"
    require "licensed/sources/yarn"
  end
end

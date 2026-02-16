# frozen_string_literal: true

module Licensee
  module ProjectFiles
    class PackageManagerFile < Licensee::ProjectFiles::ProjectFile
      # Hash of Extension => [possible matchers]
      MATCHERS_EXTENSIONS = {
        '.gemspec' => [Matchers::Gemspec],
        '.json'    => [Matchers::NpmBower],
        '.cabal'   => [Matchers::Cabal],
        '.nuspec'  => [Matchers::NuGet]
      }.freeze

      # Hash of Filename => [possible matchers]
      FILENAMES_EXTENSIONS = {
        'DESCRIPTION'  => [Matchers::Cran],
        'dist.ini'     => [Matchers::DistZilla],
        'LICENSE.spdx' => [Matchers::Spdx],
        'Cargo.toml'   => [Matchers::Cargo]
      }.freeze

      FILENAMES_SCORES = {
        'package.json'     => 1.0,
        'LICENSE.spdx'     => 1.0,
        'Cargo.toml'       => 1.0,
        'DESCRIPTION'      => 0.9,
        'dist.ini'         => 0.8,
        'bower.json'       => 0.75,
        'elm-package.json' => 0.7
      }.freeze

      def possible_matchers
        MATCHERS_EXTENSIONS[extension] || FILENAMES_EXTENSIONS[filename] || []
      end

      def self.name_score(filename)
        return 1.0 if ['.gemspec', '.cabal', '.nuspec'].include?(File.extname(filename))

        FILENAMES_SCORES[filename] || 0.0
      end

      private

      def extension
        @extension ||= File.extname(filename)
      end
    end
  end
end

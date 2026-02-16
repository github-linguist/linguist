# frozen_string_literal: true

RSpec.describe Licensee::ProjectFiles::PackageManagerFile do
  subject { described_class.new(content, filename) }

  let(:content) { '' }
  let(:filename) { '' }

  context 'name scoring' do
    {
      'licensee.gemspec' => 1.0,
      'test.cabal'       => 1.0,
      'package.json'     => 1.0,
      'Cargo.toml'       => 1.0,
      'DESCRIPTION'      => 0.9,
      'dist.ini'         => 0.8,
      'bower.json'       => 0.75,
      'elm-package.json' => 0.70,
      'README.md'        => 0.0
    }.each do |filename, expected_score|
      context "a file named #{filename}" do
        let(:score) { described_class.name_score(filename) }

        it 'scores the file' do
          expect(score).to eql(expected_score)
        end
      end
    end
  end

  context 'matchers' do
    let(:possible_matchers) { subject.possible_matchers }

    context 'with a gemspec' do
      let(:filename) { 'project.gemspec' }

      it 'returns the gemspec matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::Gemspec])
      end
    end

    context 'with cabal file' do
      let(:filename) { 'test.cabal' }

      it 'returns the cabal matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::Cabal])
      end
    end

    context 'with package.json' do
      let(:filename) { 'package.json' }

      it 'returns the gemspec matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::NpmBower])
      end
    end

    context 'with dist.ini' do
      let(:filename) { 'dist.ini' }

      it 'returns the DistZilla matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::DistZilla])
      end
    end

    context 'with DESCRIPTION' do
      let(:filename) { 'DESCRIPTION' }
      let(:content) { 'Package: test' }

      it 'returns the Cran matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::Cran])
      end
    end

    context 'with nuspec file' do
      let(:filename) { 'foo.nuspec' }

      it 'returns the NuGet matcher' do
        expect(possible_matchers).to eql([Licensee::Matchers::NuGet])
      end
    end
  end
end

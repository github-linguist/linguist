# frozen_string_literal: true

RSpec.describe Licensee::ProjectFiles::ReadmeFile do
  subject { described_class.new(content, filename) }

  let(:filename) { 'README.md' }
  let(:content) { '' }

  context 'scoring names' do
    {
      'readme'       => 1.0,
      'README'       => 1.0,
      'readme.md'    => 0.9,
      'README.md'    => 0.9,
      'readme.txt'   => 0.9,
      'readme.mdown' => 0.9,
      'readme.rdoc'  => 0.9,
      'readme.rst'   => 0.9,
      'LICENSE'      => 0.0
    }.each do |filename, expected_score|
      context "with a file named #{filename}" do
        let(:score) { described_class.name_score(filename) }

        it 'scores the file' do
          expect(score).to eql(expected_score)
        end
      end
    end
  end

  context 'parsing license content' do
    let(:license) { described_class.license_content(content) }

    context 'with no license' do
      let(:content) { 'There is no License in this README' }

      it 'returns no content' do
        expect(license).to be_nil
      end
    end

    context 'after an H1' do
      let(:content) { "# License\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'after an H2' do
      let(:content) { "## License\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'after an underlined header' do
      let(:content) { "License\n-------\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'With a strangely cased heading' do
      let(:content) { "## LICENSE\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'With british spelling' do
      let(:content) { "## Licence\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'with trailing content' do
      let(:content) { "## License\n\nhello world\n\n# Contributing" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'with trailing content that has an underlined header' do
      let(:content) { "# License\n\nhello world\n\nContributing\n====" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'with trailing content that has a hashes-based header' do
      let(:content) { "# License\n\nhello world\n\n# Contributing" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'With a trailing colon' do
      let(:content) { "## License:\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'With trailing hashes' do
      let(:content) { "## License ##\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end

    context 'Rdoc' do
      let(:content) { "== License:\n\nhello world" }

      it 'returns the license' do
        expect(license).to eql('hello world')
      end
    end
  end

  context 'a license reference' do
    let(:content) { 'The MIT License' }
    let(:mit) { Licensee::License.find('mit') }

    it 'matches' do
      expect(subject.match).to eql(mit)
    end
  end
end

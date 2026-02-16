# frozen_string_literal: true

RSpec.describe Licensee::License do
  let(:license_count) { 49 }
  let(:hidden_license_count) { 36 }
  let(:featured_license_count) { 3 }
  let(:pseudo_license_count) { 2 }
  let(:non_featured_license_count) do
    license_count - featured_license_count - hidden_license_count
  end

  let(:mit) { described_class.find('mit') }
  let(:cc_by) { described_class.find('cc-by-4.0') }
  let(:unlicense) { described_class.find('unlicense') }
  let(:other) { described_class.find('other') }
  let(:no_license) { described_class.find('no-license') }
  let(:gpl) { described_class.find('gpl-3.0') }
  let(:lgpl) { described_class.find('lgpl-3.0') }
  let(:content_hash) { license_hashes['mit'] }

  let(:license_dir) do
    File.expand_path 'vendor/choosealicense.com/_licenses', project_root
  end

  context 'listing licenses' do
    let(:licenses) { described_class.all(arguments) }

    it 'returns the license keys' do
      expect(described_class.keys.count).to eql(license_count)
      expect(described_class.keys).to include(mit.key)
      expect(described_class.keys).to include('other')
    end

    context 'without any arguments' do
      let(:arguments) { {} }

      it 'returns the licenses' do
        expect(licenses).to all be_a(described_class)
        expect(licenses.count).to eql(license_count - hidden_license_count)
      end

      it "doesn't include hidden licenses" do
        expect(licenses).to all(satisfy { |license| !license.hidden? })
      end

      it 'includes featured licenses' do
        expect(licenses).to include(mit)
        expect(licenses).not_to include(cc_by)
        expect(licenses).not_to include(other)
      end
    end

    context 'hidden licenses' do
      let(:arguments) { { hidden: true } }

      it 'includes hidden licenses' do
        expect(licenses).to include(cc_by)
        expect(licenses).to include(mit)
        expect(licenses.count).to eql(license_count)
      end
    end

    context 'featured licenses' do
      let(:arguments) { { featured: true } }

      it 'includes only featured licenses' do
        expect(licenses).to include(mit)
        expect(licenses).not_to include(cc_by)
        expect(licenses).not_to include(other)
        expect(licenses.count).to eql(featured_license_count)
      end
    end

    context 'non-featured licenses' do
      let(:arguments) { { featured: false } }

      it 'includes only non-featured licenses' do
        expect(licenses).to include(unlicense)
        expect(licenses).not_to include(mit)
        expect(licenses).not_to include(other)
        expect(licenses.count).to eql(non_featured_license_count)
      end

      context 'including hidden licenses' do
        let(:arguments) { { featured: false, hidden: true } }

        it 'includes only non-featured licenses' do
          expect(licenses).to include(unlicense)
          expect(licenses).to include(cc_by)
          expect(licenses).not_to include(mit)
          expect(licenses.count).to eql(license_count - featured_license_count)
        end
      end
    end

    context 'pseudo licenses' do
      let(:other) { described_class.find('other') }

      context 'by default' do
        let(:arguments) { {} }

        it "doesn't include pseudo licenses" do
          expect(licenses).not_to include(other)
        end
      end

      context 'with hidden licenses' do
        let(:arguments) { { hidden: true } }

        it 'includes pseudo licenses' do
          expect(licenses).to include(other)
        end
      end

      context 'when explicitly asked' do
        let(:arguments) { { hidden: true, pseudo: true } }

        it 'includes psudo licenses' do
          expect(licenses).to include(other)
        end
      end

      context 'when explicitly excluded' do
        let(:arguments) { { hidden: true, pseudo: false } }

        it "doesn'tincludes psudo licenses" do
          expect(licenses).not_to include(other)
        end
      end

      context 'mispelled' do
        context 'when explicitly asked' do
          let(:arguments) { { hidden: true, psuedo: true } }

          it 'includes psudo licenses' do
            expect(licenses).to include(other)
          end
        end

        context 'when explicitly excluded' do
          let(:arguments) { { hidden: true, psuedo: false } }

          it "doesn'tincludes psudo licenses" do
            expect(licenses).not_to include(other)
          end
        end
      end
    end
  end

  context 'finding' do
    it 'finds the MIT license' do
      expect(described_class.find('mit')).to eql(mit)
    end

    it 'finds hidden licenses' do
      expect(described_class.find('cc-by-4.0')).to eql(cc_by)
    end

    it 'is case insensitive' do
      expect(described_class.find('MIT')).to eql(mit)
    end
  end

  it 'returns the license dir' do
    expect(described_class.license_dir).to eql(license_dir)
    expect(described_class.license_dir).to be_an_existing_file
  end

  it 'returns license files' do
    expected = license_count - pseudo_license_count
    expect(described_class.license_files.count).to eql(expected)
    expect(described_class.license_files).to all be_an_existing_file
    expect(described_class.license_files).to include(mit.path)
  end

  it 'stores the key when initialized' do
    expect(described_class.new('mit')).to be == mit
    expect(described_class.new('MIT')).to be == mit
  end

  it 'exposes the path' do
    expect(mit.path).to be_an_existing_file
    expect(mit.path).to match(described_class.license_dir)
  end

  it 'exposes the key' do
    expect(mit.key).to eql('mit')
  end

  it 'exposes the SPDX ID' do
    expect(gpl.spdx_id).to eql('GPL-3.0')
  end

  it 'exposes special SPDX ID for pseudo licenses' do
    expect(other.spdx_id).to eql('NOASSERTION')
    expect(no_license.spdx_id).to eql('NONE')
  end

  describe '#other?' do
    it 'knows MIT is not other' do
      expect(gpl).not_to be_other
    end

    it 'knows the other license is other?' do
      expect(other).to be_other
    end
  end

  context 'meta' do
    it 'exposes license meta' do
      expect(mit).to respond_to(:meta)
      expect(mit.meta).to respond_to(:title)
      expect(mit.meta['title']).to eql('MIT License')
      expect(mit.meta.title).to eql('MIT License')
    end

    it 'includes defaults' do
      expect(other.meta['hidden']).to be(true)
    end

    it 'returns the name' do
      expect(mit.name).to eql('MIT License')
    end

    it 'uses the default name when none exists' do
      expect(other.name).to eql('Other')
      expect(no_license.name).to eql('No license')
    end

    it 'expoeses the nickname' do
      expect(gpl.nickname).to eql('GNU GPLv3')
    end

    it 'exposes the name without version' do
      expect(mit.name_without_version).to eql('MIT License')
      expect(gpl.name_without_version).to eql('GNU General Public License')
    end

    it 'knows if a license is hidden' do
      expect(mit).not_to be_hidden
      expect(cc_by).to be_hidden
    end

    it 'knows if a license is featured' do
      expect(mit).to be_featured
      expect(unlicense).not_to be_featured
    end

    it 'knows if a license is GPL' do
      expect(mit).not_to be_gpl
      expect(gpl).to be_gpl
    end

    it 'knows a license is lgpl' do
      expect(mit).not_to be_gpl
      expect(lgpl).to be_lgpl
    end

    it 'knows if a license is CC' do
      expect(gpl).not_to be_creative_commons
      expect(cc_by).to be_creative_commons
    end
  end

  context 'content' do
    it 'returns the license content' do
      expect(mit.content).to match('Permission is hereby granted')
    end

    it 'strips leading whitespace' do
      expect(mit.content).to start_with('M')
    end

    it 'computes the hash' do
      expect(mit.content_hash).to eql(content_hash)
    end

    context 'with content stubbed' do
      let(:license) do
        license = described_class.new 'MIT'
        license.instance_variable_set(:@raw_content, content)
        license
      end

      context 'with a horizontal rule' do
        let(:content) do
          "---\nfoo: bar\n---\nSome license\n---------\nsome text\n"
        end

        it 'parses the content' do
          expect(license.content).to eql("Some license\n---------\nsome text\n")
        end
      end
    end
  end

  it 'returns the URL' do
    expect(mit.url).to eql('http://choosealicense.com/licenses/mit/')
  end

  it 'knows equality' do
    expect(described_class.find('mit')).to eql(mit)
    expect(described_class.find('mit')).to eq(mit)
    expect(gpl).not_to eql(mit)
  end

  it 'returns false when compared to a boolean' do
    expect(described_class.find('mit')).not_to be(true)
  end

  it 'knows if a license is a pseudo license' do
    expect(mit).not_to be_pseudo_license
    expect(other).to be_pseudo_license
  end

  it 'fails loudly for invalid license' do
    expect do
      described_class.new('foo').name
    end.to raise_error(Licensee::InvalidLicense)
  end

  it 'returns the rules' do
    expect(mit.rules).to be_a(Licensee::LicenseRules)
    expect(mit.rules).to have_key('permissions')
    expect(mit.rules['permissions'].first).to be_a(Licensee::Rule)
    expect(mit.rules.flatten.count).to be(7)
  end

  it 'returns rules by tag and group' do
    expect(cc_by.rules).to have_key('limitations')
    rule = cc_by.rules['limitations'].find { |r| r.tag == 'patent-use' }
    expect(rule).not_to be_nil
    expect(rule.description).to include('does NOT grant')

    expect(gpl.rules).to have_key('permissions')
    rule = gpl.rules['permissions'].find { |r| r.tag == 'patent-use' }
    expect(rule).not_to be_nil
    expect(rule.description).to include('an express grant of patent rights')
  end

  context 'fields' do
    it 'returns the license fields' do
      expect(mit.fields.count).to be(2)
      expect(mit.fields.first.key).to eql('year')
      expect(mit.fields.last.key).to eql('fullname')
      expect(gpl.fields).to be_empty
    end

    context 'muscache' do
      let(:license) do
        license = described_class.new 'MIT'
        content = "#{license.content}[foo] [bar]"
        license.instance_variable_set(:@content, content)
        license
      end

      it 'returns mustache content' do
        expect(license.content_for_mustache).to match(/{{{year}}}/)
        expect(license.content_for_mustache).to match(/{{{fullname}}}/)
        expect(license.content_for_mustache).not_to match(/\[year\]/)
        expect(license.content_for_mustache).not_to match(/\[fullname\]/)
      end

      it "doesn't mangle other fields" do
        expect(license.content_for_mustache).to match(/\[foo\]/)
        expect(license.content_for_mustache).not_to match(/{{{foo}}}/)
      end
    end
  end

  context 'License.title_regex' do
    namey = %i[title nickname key]
    described_class.all(hidden: true, pseudo: false).each do |license|
      context "the #{license.title} license" do
        namey.each do |variation|
          next if license.send(variation).nil?

          context "the license #{variation}" do
            let(:license_variation) { license.send(variation).sub('*', 'u') }
            let(:text) { license_variation }

            it 'matches' do
              expect(text).to match(license.title_regex)
            end

            it 'finds by title' do
              expect(described_class.find_by_title(text)).to eql(license)
            end

            if /\bGNU\b/.match?(license.title)
              context "without 'GNU'" do
                let(:text) { license_variation.sub(/GNU /i, '') }

                it 'still matches' do
                  expect(text).to match(license.title_regex)
                end
              end
            end

            context "with 'the' and 'license'" do
              let(:text) { "The #{license_variation} license" }

              it 'matches' do
                expect(text).to match(license.title_regex)
              end
            end

            if variation == :title
              context 'version notation variations' do
                context "with 'version x.x'" do
                  let(:text) do
                    license_variation.sub(/v?(\d+\.\d+)/i, 'version \1')
                  end

                  it 'matches' do
                    expect(text).to match(license.title_regex)
                  end
                end

                context "with ', version x.x'" do
                  let(:text) do
                    license_variation.sub(/ v?(\d+\.\d+)/i, ', version \1')
                  end

                  it 'matches' do
                    expect(text).to match(license.title_regex)
                  end
                end

                context "with 'vx.x'" do
                  let(:text) do
                    license_variation.sub(/(?:version)? (\d+\.\d+)/i, ' v\1')
                  end

                  it 'matches' do
                    expect(text).to match(license.title_regex)
                  end
                end
              end
            end
          end
        end
      end
    end

    context 'a license with an alt title' do
      let(:text) { 'The Clear BSD license' }
      let(:license) { described_class.find('bsd-3-clause-clear') }

      it 'matches' do
        expect(text).to match(license.title_regex)
      end

      it 'finds by title' do
        expect(described_class.find_by_title(text)).to eql(license)
      end
    end
  end

  context 'to_h' do
    let(:hash) { mit.to_h }
    let(:expected) do
      {
        key:     'mit',
        spdx_id: 'MIT',
        meta:    mit.meta.to_h,
        url:     'http://choosealicense.com/licenses/mit/',
        rules:   mit.rules.to_h,
        fields:  mit.fields.map(&:to_h),
        other:   false,
        gpl:     false,
        lgpl:    false,
        cc:      false
      }
    end

    it 'Converts to a hash' do
      expect(hash).to eql(expected)
    end
  end

  context 'source regex' do
    schemes = %w[http https]
    prefixes = ['www.', '']
    suffixes = ['.html', '.htm', '.txt', '']
    described_class.all(hidden: true, pseudo: false).each do |license|
      context "the #{license.title} license" do
        let(:source) { URI.parse(license.source) }

        schemes.each do |scheme|
          context "with a #{scheme}:// scheme" do
            before { source.scheme = scheme }

            prefixes.each do |prefix|
              context "with '#{prefix}' before the host" do
                before do
                  source.host = "#{prefix}#{source.host.delete_prefix('www.')}"
                end

                suffixes.each do |suffix|
                  context "with '#{suffix}' after the path" do
                    before do
                      next if license.key == 'wtfpl'

                      regex = /#{Licensee::License::SOURCE_SUFFIX}\z/o
                      source.path = source.path.sub(regex, '')
                      source.path = "#{source.path}#{suffix}"
                    end

                    it 'matches' do
                      expect(source.to_s).to match(license.source_regex)
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end

# frozen_string_literal: true

RSpec.describe 'vendored licenses' do
  let(:filename) { 'LICENSE.txt' }
  let(:license_file) do
    Licensee::ProjectFiles::LicenseFile.new(content, filename)
  end

  Licensee.licenses(hidden: true).each do |license|
    next if license.pseudo_license?

    context "the #{license.name} license" do
      let(:content_with_copyright) { sub_copyright_info(license) }
      let(:content) { content_with_copyright }
      let(:expected_hash) { license_hashes[license.key] }
      let(:hash_change_msg) do
        msg = +'Did you update a vendored license? Run '
        msg << '`bundle exec script/hash-licenses`. '
        msg << 'Changes in license hashes must be a MINOR (or MAJOR) bump.'
        msg
      end

      it 'detects the license' do
        expect(content).to be_detected_as(license)
      end

      it 'confidence and similarity scores are euqal' do
        expect(license_file.confidence).to eq(license.similarity(license_file))
      end

      it 'has a cached content hash' do
        expect(expected_hash).not_to be_nil, hash_change_msg
      end

      it 'matches the expected content hash' do
        expect(license.content_hash).to eql(expected_hash), hash_change_msg
      end

      context 'when modified' do
        let(:line_length) { 60 }
        let(:random_words) { 75 }
        let(:content_rewrapped) do
          Licensee::ContentHelper.wrap(content_with_copyright, line_length)
        end
        let(:content_with_random_words) do
          add_random_words(content_with_copyright, random_words)
        end

        context 'without the title' do
          let(:content_without_title) do
            license_file.send :strip_title
            license_file.send :_content
          end

          it 'detects the license' do
            expect(content_without_title).to be_detected_as(license)
          end
        end

        context 'with a double title' do
          let(:content) do
            "#{license.name.sub('*', 'u')}\n\n#{content_with_copyright}"
          end

          it 'detects the license' do
            expect(content).to be_detected_as(license)
          end
        end

        context 'when re-wrapped' do
          let(:content) { content_rewrapped }

          it 'detects the license' do
            expect(content).to be_detected_as(license)
          end
        end

        context 'with random words added' do
          let(:content) { content_with_random_words }

          it 'does not match the license' do
            expect(content).not_to be_detected_as(license)
          end
        end

        context 'when rewrapped with random words added' do
          let(:content) do
            Licensee::ContentHelper.wrap(content_with_random_words, line_length)
          end

          it 'does not match the license' do
            expect(content).not_to be_detected_as(license)
          end
        end
      end
    end
  end
end

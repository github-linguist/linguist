# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Exact do
  subject { described_class.new(file) }

  let(:mit) { Licensee::License.find('mit') }
  let(:content) { sub_copyright_info(mit) }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }

  it 'stores the file' do
    expect(subject.file).to eql(file)
  end

  it 'matches' do
    expect(content).to be_detected_as(mit)
  end

  it 'is confident' do
    expect(subject.confidence).to be(100)
  end

  context 'with extra words added' do
    let(:content) { add_random_words(mit.content) }

    it "doesn't match" do
      expect(subject.match).to be_nil
    end
  end
end

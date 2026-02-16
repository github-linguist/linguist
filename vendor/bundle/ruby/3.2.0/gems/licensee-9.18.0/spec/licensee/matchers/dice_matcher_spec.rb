# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Dice do
  subject { described_class.new(file) }

  let(:mit) { Licensee::License.find('mit') }
  let(:gpl) { Licensee::License.find('gpl-3.0') }
  let(:agpl) { Licensee::License.find('agpl-3.0') }
  let(:lgpl) { Licensee::License.find('lgpl-2.1') }
  let(:cc_by) { Licensee::License.find('cc-by-4.0') }
  let(:cc_by_sa) { Licensee::License.find('cc-by-sa-4.0') }
  let(:content) { sub_copyright_info(gpl) }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }

  it 'stores the file' do
    expect(subject.file).to eql(file)
  end

  it 'matches' do
    expect(subject.match).to eql(gpl)
  end

  it 'sorts licenses by similarity' do
    expect(subject.matches_by_similarity[0]).to eql([gpl, 100.0])
    expect(subject.matches_by_similarity[1]).to eql([agpl, 94.56967213114754])
    expect(subject.matches_by_similarity[2]).to eql([lgpl, 26.821370750134918])
  end

  it 'returns the match confidence' do
    expect(subject.confidence).to eq(100.0)
  end

  context 'without a match' do
    let(:content) { 'Not really a license' }

    it "doesn't match" do
      expect(subject.match).to be_nil
      expect(subject.matches).to be_empty
      expect(subject.confidence).to be(0)
    end
  end

  context 'stacked licenses' do
    let(:content) do
      "#{sub_copyright_info(mit)}\n\n#{sub_copyright_info(gpl)}"
    end

    it "doesn't match" do
      expect(content).not_to be_detected_as(gpl)
      expect(subject.match).to be_nil
      expect(subject.matches).to be_empty
      expect(subject.confidence).to be(0)
    end
  end

  context 'CC false positive' do
    context 'CC-BY' do
      let(:content) { cc_by.content }

      it 'matches' do
        expect(content).to be_detected_as(cc_by)
      end
    end

    context 'CC-ND' do
      let(:project_path) { fixture_path('cc-by-nd') }
      let(:license_path) { File.expand_path('LICENSE', project_path) }
      let(:content) { File.read(license_path) }

      it "doesn't match" do
        expect(content).not_to be_detected_as(cc_by)
        expect(content).not_to be_detected_as(cc_by_sa)
        expect(subject.match).to be_nil
        expect(subject.matches).to be_empty
        expect(subject.confidence).to be(0)
      end
    end
  end
end

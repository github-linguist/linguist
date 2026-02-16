# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Package do
  subject { described_class.new(file) }

  let(:mit) { Licensee::License.find('mit') }
  let(:content) { '' }
  let(:file) do
    Licensee::ProjectFiles::LicenseFile.new(content, 'project.gemspec')
  end
  let(:license_property) { 'mit' }

  before do
    allow(subject).to receive(:license_property).and_return(license_property)
  end

  it 'matches' do
    expect(subject.match).to eql(mit)
  end

  it 'has confidence' do
    expect(subject.confidence).to be(90)
  end

  context 'with a nil license property' do
    let(:license_property) { nil }

    it 'matches to nil' do
      expect(subject.match).to be_nil
    end
  end

  context 'with an empty license property' do
    let(:license_property) { '' }

    it 'matches to nil' do
      expect(subject.match).to be_nil
    end
  end

  context 'with an unmatched license proprerty' do
    let(:license_property) { 'foo' }

    it 'matches to other' do
      expect(subject.match).to eql(Licensee::License.find('other'))
    end
  end
end

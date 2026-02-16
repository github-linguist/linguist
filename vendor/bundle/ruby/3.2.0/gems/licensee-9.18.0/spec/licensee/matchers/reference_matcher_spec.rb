# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Reference do
  subject { described_class.new(file) }

  let(:content) { 'Copyright 2015 Ben Balter' }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }
  let(:license) { Licensee::License.find('gpl-3.0') }

  %i[title key nickname].each do |variation|
    context "with a license #{variation}" do
      let(:content) { "Licensed under the #{license.send(variation)} license" }

      it 'matches' do
        expect(subject.match).to eql(license)
      end

      context 'as a markdown link' do
        let(:content) { "[#{license.send(variation)}](https://example.com)" }

        it 'matches' do
          expect(subject.match).to eql(license)
        end
      end
    end
  end

  context 'a license key in a word' do
    let(:content) { 'My name is MITch!' }

    it "doesn't match" do
      expect(subject.match).to be_nil
    end
  end

  context 'a license with alt regex' do
    let(:content) { 'Clear BSD' }
    let(:license) { Licensee::License.find('bsd-3-clause-clear') }

    it 'matches' do
      expect(subject.match).to eql(license)
    end
  end

  context 'with a license source' do
    let(:license) { Licensee::License.find('mpl-2.0') }
    let(:content) { "The [license](#{license.source})" }

    it 'matches' do
      expect(subject.match).to eql(license)
    end
  end

  context 'x.0 license sans .0' do
    let(:content) { 'Apache V2' }
    let(:license) { Licensee::License.find('apache-2.0') }

    it 'matches' do
      expect(subject.match).to eql(license)
    end
  end

  context 'x.1 license sans .1' do
    let(:content) { 'EUPL-1' }

    it 'matches' do
      expect(subject.match).to be_nil
    end
  end
end

# frozen_string_literal: true

RSpec.describe Licensee::Matchers::Copyright do
  subject { described_class.new(file) }

  let(:content) { 'Copyright 2015 Ben Balter'.dup }
  let(:file) { Licensee::ProjectFiles::LicenseFile.new(content, 'LICENSE.txt') }
  let(:mit) { Licensee::License.find('mit') }
  let(:no_license) { Licensee::License.find('no-license') }

  it 'stores the file' do
    expect(subject.file).to eql(file)
  end

  it 'matches' do
    expect(subject.match).to eql(no_license)
  end

  it 'has a confidence' do
    expect(subject.confidence).to be(100)
  end

  {
    'Standard'              => 'Copyright (C) 2015 Ben Balter',
    'Unicode (C)-style'     => 'Copyright © 2015 Ben Balter',
    'Symbol only'           => '(C) 2015 Ben Balter',
    'UTF-8 Encoded'         => 'Copyright (c) 2010-2014 Simon Hürlimann',
    'Comma-separated date'  => 'Copyright (c) 2003, 2004 Ben Balter',
    'Hyphen-separated date' => 'Copyright (c) 2003-2004 Ben Balter',
    'ASCII-8BIT encoded'    => "Copyright \xC2\xA92015 Ben Balter`"
      .dup.force_encoding('ASCII-8BIT'),
    'No year'               => 'Copyright Ben Balter',
    'Multiline'             => "Copyright Ben Balter\nCopyright Another Entity",
    'OFL font name'         => "Copyright (c) 2016, Ben Balter,\nwith Reserved Font Name \"Ben's Font\"."
  }.each do |description, notice|
    context "with a #{description} notice" do
      let(:content) { notice }

      it 'matches' do
        expect(content).to be_detected_as(no_license)
      end
    end
  end

  context 'with arbitrary additional notice line' do
    let(:content) { "(c) Ben Balter\nwith foo bar baz" }

    it "doesn't match" do
      expect(subject.match).to be_nil
    end
  end

  context 'with a license with a copyright notice' do
    let(:content) { sub_copyright_info(mit) }

    it "doesn't match" do
      expect(subject.match).to be_nil
    end
  end
end

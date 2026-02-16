# frozen_string_literal: true

class ContentHelperTestHelper
  include Licensee::ContentHelper
  attr_accessor :content, :data

  def initialize(content = nil, data = {})
    @content = content
    @data = data
  end

  def filename
    @data[:filename]
  end

  def spdx_id
    @data[:spdx_id]
  end
end

RSpec.describe Licensee::ContentHelper do
  subject { ContentHelperTestHelper.new(content, filename: filename, spdx_id: spdx_id) }

  let(:content) do
    <<-LICENSE.gsub(/^\s*/, '')
  # The MIT License
	=================

	Copyright 2016 Ben Balter
	*************************

  All rights reserved.

  The made
  * * * *
  up  license.

  This license provided 'as is'. Please respect the contributors' wishes when
  implementing the license's "software".
  -----------
    LICENSE
  end
  let(:filename) { 'license.md' }
  let(:spdx_id) { 'MIT' }

  let(:mit) { Licensee::License.find('mit') }
  let(:normalized_content) { subject.content_normalized }

  it 'creates the wordset' do
    wordset = Set.new(
      %w[
        the made up license this provided as is' please respect
        contributors' wishes when implementing license's software
      ]
    )
    expect(subject.wordset).to eql(wordset)
  end

  it 'knows the length' do
    expect(subject.length).to be(135)
  end

  it 'knows the length delta' do
    expect(subject.length_delta(mit)).to be(885)
    expect(subject.length_delta(subject)).to be(0)
  end

  it 'knows the similarity' do
    expect(mit.similarity(subject)).to be_within(1).of(4)
    expect(mit.similarity(mit)).to eq(100.0)
  end

  it 'calculates simple delta for similarity' do
    expect(subject.similarity(mit)).to be_within(1).of(3)
  end

  it 'calculates the hash' do
    content_hash = '9b4bed43726cf39e17b11c2942f37be232f5709a'
    expect(subject.content_hash).to eql(content_hash)
  end

  it 'wraps' do
    content = described_class.wrap(mit.content, 40)
    lines = content.split("\n")
    expect(lines.first.length).to be <= 40
  end

  it 'formats percents' do
    percent = described_class.format_percent(12.3456789)
    expect(percent).to eql('12.35%')
  end

  describe '#strip' do
    {
      version:             "The MIT License\nVersion 1.0\nfoo",
      hrs:                 "The MIT License\n=====\n-----\n*******\nfoo",
      markdown_headings:   "# The MIT License\n\nfoo",
      whitespace:          "The MIT License\n\n   foo  ",
      all_rights_reserved: "Copyright 2016 Ben Balter\n\nfoo",
      urls:                "https://example.com\nfoo",
      developed_by:        "Developed By: Ben Balter\n\nFoo",
      borders:             '*   Foo    *',
      title:               "The MIT License\nfoo",
      copyright:           "The MIT License\nCopyright 2018 Ben Balter\nFoo",
      copyright_bullet:    "The MIT License\n* Copyright 2018 Ben Balter\nFoo",
      copyright_italic:    "The MIT License\n_Copyright 2018 Ben Balter_\nFoo",
      end_of_terms:        "Foo\nend of terms and conditions\nbar",
      end_of_terms_hashes: "Foo\n# end of terms and conditions ####\nbar",
      block_markup:        '> Foo',
      link_markup:         '[Foo](http://exmaple.com)',
      comment_markup:      "/*\n* The MIT License\n* Foo\n*/",
      copyright_title:     "Copyright 2019 Ben Balter\nMIT License\nFoo"
    }.each do |field, fixture|
      describe "#strip_#{field}" do
        let(:content) { fixture }

        it "strips #{field}" do
          expect(normalized_content).to eql('foo')
        end
      end
    end

    context 'span markup' do
      let(:content) { '_foo_ *foo* **foo** ~foo~' }

      it 'strips span markup' do
        expect(normalized_content).to eql('foo foo foo foo')
      end
    end

    context 'HTML' do
      let(:content) { '<ul><li>foo</li></ul>' }
      let(:filename) { 'license.html' }

      it 'strips HTML' do
        expect(normalized_content).to eql('- foo')
      end
    end
  end

  context 'integration fixture' do
    it 'strips copyright' do
      expect(normalized_content).not_to match 'Copyright'
      expect(normalized_content).not_to match 'Ben Balter'
    end

    it 'downcases' do
      expect(normalized_content).not_to match 'The'
      expect(normalized_content).to match 'the'
    end

    it 'strips HRs' do
      expect(normalized_content).not_to match '---'
      expect(normalized_content).not_to match '==='
      expect(normalized_content).not_to include '***'
      expect(normalized_content).not_to include '* *'
    end

    it 'squeezes whitespace' do
      expect(normalized_content).not_to match '  '
    end

    it 'strips whitespace' do
      expect(normalized_content).not_to match(/\n/)
      expect(normalized_content).not_to match(/\t/)
    end

    it 'strips markdown headings' do
      expect(normalized_content).not_to match('#')
    end

    it 'strips all rights reserved' do
      expect(normalized_content).not_to match(/all rights reserved/i)
    end

    it 'strips markup' do
      expect(normalized_content).not_to match(/[*=_-]+/)
    end

    it 'normalizes quotes' do
      expect(normalized_content).not_to match('"as is"')
    end

    it 'preserves possessives' do
      expect(normalized_content).to match("contributors'")
      expect(normalized_content).to match("license's")
    end

    it 'strips the title' do
      expect(normalized_content).not_to match('MIT')
    end

    it 'normalize the content' do
      expected = +"the made up license. this license provided 'as is'. "
      expected << "please respect the contributors' wishes when implementing "
      expected << "the license's 'software'."
      expect(normalized_content).to eql(expected)
    end
  end

  context 'normalizing' do
    context 'https' do
      let(:content) { 'http://example.com' }

      it 'normalized URL protocals' do
        expect(subject.content_normalized).to eql('https://example.com')
      end
    end

    context 'ampersands' do
      let(:content) { 'Foo & Bar' }

      it 'normalized ampersands' do
        expect(subject.content_normalized).to eql('foo and bar')
      end
    end

    context 'lists' do
      let(:content) { "1. Foo\n * Bar" }

      it 'normalizes lists' do
        expect(subject.content_normalized).to eql('- foo - bar')
      end
    end

    context 'lists with formatting bullets' do
      let(:content) { "- **(a)** Foo\n * b) Bar" }

      it 'normalizes lists' do
        expect(subject.content_normalized).to eql('- foo - bar')
      end
    end

    context 'dashes' do
      let(:content) { 'Foo-Bar—–baz-buzz' }

      it 'normalizes dashes' do
        expect(subject.content_normalized).to eql('foo-bar-baz-buzz')
      end
    end

    context 'hyphenated across lines' do
      let(:content) { "cc-\nlicensed" }

      it 'normalized hyphenated across lines' do
        expect(subject.content_normalized).to eql('cc-licensed')
      end
    end

    context 'quotes' do
      let(:content) { "`a` 'b' \"c\" ‘d’ “e”" }

      it 'normalizes quotes' do
        expect(subject.content_normalized).to eql("'a' 'b' 'c' 'd' 'e'")
      end
    end

    it 'strips formatting from the MPL' do
      license = Licensee::License.find('mpl-2.0')
      expect(license.content_normalized).not_to include('* *')
    end

    it 'normalizes http: to https:' do
      license = Licensee::License.find('ofl-1.1')
      expect(license.content).to include('http:')
      expect(license.content_normalized).not_to include('http:')
    end

    it 'wraps' do
      lines = mit.content_normalized(wrap: 40).split("\n")
      expect(lines.first.length).to be <= 40
    end

    context 'spelling' do
      let(:content) { 'licence' }

      it 'normalizes' do
        expect(subject.content_normalized).to eql('license')
      end
    end

    Licensee::License.all(hidden: true).each do |license|
      context license.name do
        let(:stripped_content) { subject.content_without_title_and_version }

        it 'strips the title' do
          regex = /\A#{license.name_without_version}/i
          expect(stripped_content).not_to match(regex)
          expect(license.content_normalized).not_to match(regex)
        end

        it 'strips the version' do
          expect(license.content_normalized).not_to match(/\Aversion/i)
          expect(stripped_content).not_to match(/\Aversion/i)
        end

        it 'strips all rights reserved' do
          regex = /all rights reserved/i
          expect(license.content_normalized).not_to match(regex)
        end

        it 'strips the copyright' do
          expect(license.content_normalized).not_to match(/\Acopyright/i)
        end

        it 'strips the implementation instructions' do
          end_terms_regex = /END OF TERMS AND CONDITIONS/i
          expect(license.content_normalized).not_to match(end_terms_regex)
          expect(license.content_normalized).not_to match(/How to apply/i)
        end
      end
    end

    context 'a title in parenthesis' do
      let(:content) { "(The MIT License)\n\nfoo" }

      it 'strips the title' do
        expect(normalized_content).not_to match('MIT')
        expect(normalized_content).to eql('foo')
      end
    end

    context 'multiple copyrights' do
      let(:content) { "Copyright 2016 Ben Balter\nCopyright 2017 Bob\nFoo" }

      it 'strips multiple copyrights' do
        expect(normalized_content).not_to match('Ben')
        expect(normalized_content).to eql('foo')
      end
    end
  end

  context 'title regex' do
    let(:license) { Licensee::License.find('gpl-3.0') }

    %i[key title nickname name_without_version].each do |variation|
      context "a license #{variation}" do
        let(:license_variation) { license.send(variation) }
        let(:text) { license_variation }

        it 'matches' do
          expect(text).to match(described_class.title_regex)
        end

        context 'preceded by the' do
          let(:text) { "The #{license_variation} license" }

          it 'matches' do
            expect(text).to match(described_class.title_regex)
          end
        end

        context 'with parens' do
          let(:text) { "(#{license_variation})" }

          it 'matches' do
            expect(text).to match(described_class.title_regex)
          end
        end

        context 'with parens and a preceding the' do
          let(:text) { "(the #{license_variation} license)" }

          it 'matches' do
            expect(text).to match(described_class.title_regex)
          end
        end

        context 'with whitespace' do
          let(:text) { "     the #{license_variation} license" }

          it 'matches' do
            expect(text).to match(described_class.title_regex)
          end
        end

        context 'escaping' do
          let(:text) { 'gpl-3 0' }

          it 'escapes' do
            expect(text).not_to match(described_class.title_regex)
          end
        end

        context 'in the middle of a string' do
          let(:text) do
            "The project is not licensed under the #{license_variation} license"
          end

          it 'matches' do
            expect(text).not_to match(described_class.title_regex)
          end
        end
      end
    end
  end

  context 'metaprogramming' do
    it 'raises on invalid normalization' do
      expect { subject.send(:normalize, :foo) }.to raise_error(ArgumentError)
    end

    it 'raises on invalid strip' do
      expect { subject.send(:strip, :foo) }.to raise_error(ArgumentError)
    end

    it 'backwards compatibalizes regexes' do
      expect(described_class::WHITESPACE_REGEX).to eql(/\s+/)
    end
  end
end

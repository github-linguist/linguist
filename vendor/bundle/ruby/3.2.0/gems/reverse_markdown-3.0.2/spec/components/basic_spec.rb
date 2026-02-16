require 'spec_helper'

describe ReverseMarkdown do

  let(:input)    { File.read('spec/assets/basic.html') }
  let(:document) { Nokogiri::HTML(input) }
  subject { ReverseMarkdown.convert(input) }

  it { is_expected.to match /plain text ?\n/ }
  it { is_expected.to match /# h1\n/ }
  it { is_expected.to match /## h2\n/ }
  it { is_expected.to match /### h3\n/ }
  it { is_expected.to match /#### h4\n/ }
  it { is_expected.to match /##### h5\n/ }
  it { is_expected.to match /###### h6\n/ }

  it { is_expected.to match /_em tag content_/ }
  it { is_expected.to match /before and after empty em tags/ }
  it { is_expected.to match /before and after em tags containing whitespace/ }
  it { is_expected.to match /_double em tags_/ }
  it { is_expected.to match /_double em tags in p tag_/ }
  it { is_expected.to match /a _em with leading and trailing_ whitespace/ }
  it { is_expected.to match /a _em with extra leading and trailing_ whitespace/ }

  it { is_expected.to match /\*\*strong tag content\*\*/ }
  it { is_expected.to match /before and after empty strong tags/ }
  it { is_expected.to match /before and after strong tags containing whitespace/ }
  it { is_expected.to match /\*\*double strong tags\*\*/ }
  it { is_expected.to match /\*\*double strong tags in p tag\*\*/ }
  it { is_expected.to match /before \*\*double strong tags containing whitespace\*\* after/ }
  it { is_expected.to match /a \*\*strong with leading and trailing\*\* whitespace/ }
  it { is_expected.to match /a \*\*strong with extra leading and trailing\*\* whitespace/ }

  it { is_expected.to match /_i tag content_/ }
  it { is_expected.to match /\*\*b tag content\*\*/ }

  it { is_expected.to match /br tags become double space followed by newline  \n/ }
  #it { should match /br tags XXX  \n/ }

  it { is_expected.to match /before hr \n\* \* \*\n after hr/ }

  it { is_expected.to match /section 1\n ?\nsection 2/ }

  describe 'whitespace handling between inline elements' do
    it 'preserves whitespace (including newlines) between spans' do
      input = "<span>Hello\n</span><span>World</span>"
      result = ReverseMarkdown.convert(input)
      expect(result).to eq "Hello World"
    end

    it 'preserves whitespace between inline elements in paragraphs' do
      input = "<p><span>Hello\n</span><span>World</span></p>"
      result = ReverseMarkdown.convert(input)
      expect(result).to eq "Hello World\n\n"
    end

    it 'preserves whitespace between nested inline elements' do
      # The text "A" is nested inside <span> inside <em>, but <em> has a following sibling
      # This requires traversing up through parent nodes to find following content
      input = "<p><em><span>A\n</span></em><span>B</span></p>"
      result = ReverseMarkdown.convert(input)
      expect(result).to eq "_A_ B\n\n"
    end

    it 'preserves whitespace surrounding links' do
      # Issue #91: newlines around inline elements should become spaces
      result = ReverseMarkdown.convert("a\n<a href='1'>link</a>\nis good")
      expect(result.strip).to eq "a [link](1) is good"
    end
  end
end

require 'spec_helper'

describe ReverseMarkdown do
  let(:input)    { File.read('spec/assets/iframe.html') }
  let(:document) { Nokogiri::HTML(input) }
  subject { ReverseMarkdown.convert(input) }

  it do
    expected = <<~MD
      # Welcome to My Page

      This is a sample paragraph before the iframe.

      https://www.example.com

      This is a sample paragraph after the iframe.

    MD

    expect(subject).to eq expected
  end
end

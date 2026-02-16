require 'spec_helper'

describe ReverseMarkdown::Converters::H do
  let(:converter) { ReverseMarkdown::Converters::H.new }

  it 'merges line breaks into single line' do
    # Markdown headings can't span multiple lines, so merge them
    result = ReverseMarkdown.convert('<h1>foo<br>bar</h1>')
    expect(result.strip).to eq '# foo bar'
  end

  it 'handles multiple line breaks' do
    result = ReverseMarkdown.convert('<h2>a<br>b<br>c</h2>')
    expect(result.strip).to eq '## a b c'
  end
end

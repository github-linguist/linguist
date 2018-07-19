require 'open-uri'
require 'hpricot'

SearchResult = Struct.new(:url, :title, :content)

class SearchYahoo
  @@urlinfo = [nil, 'ca.search.yahoo.com', 80, '/search', nil, nil]

  def initialize(term)
    @term = term
    @page = 1
    @results = nil
    @url = URI::HTTP.build(@@urlinfo)
  end

  def next_result
    if not @results
      @results = []
      fetch_results
    elsif @results.empty?
      next_page
    end
    @results.shift
  end

  def fetch_results
    @url.query = URI.escape("p=%s&b=%d" % [@term, @page])
    doc = open(@url) { |f| Hpricot(f) }
    parse_html(doc)
  end

  def next_page
    @page += 10
    fetch_results
  end

  def parse_html(doc)
    doc.search("div#main").search("div").each do |div|
      next unless div.has_attribute?("class") and div.get_attribute("class").index("res") == 0
      result = SearchResult.new
      div.search("a").each do |link|
        next unless link.has_attribute?("class") and link.get_attribute("class") == "yschttl spt"
        result.url = link.get_attribute("href")
        result.title = link.inner_text
      end
      div.search("div").each do |abstract|
        next unless abstract.has_attribute?("class") and abstract.get_attribute("class").index("abstr")
        result.content = abstract.inner_text
      end
      @results << result
    end
  end
end

s = SearchYahoo.new("test")
15.times do |i|
  result = s.next_result
  puts i+1
  puts result.title
  puts result.url
  puts result.content
  puts
end

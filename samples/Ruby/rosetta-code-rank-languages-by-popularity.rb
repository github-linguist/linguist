require 'rosettacode'

langs = []
RosettaCode.category_members("Programming Languages") {|lang| langs << lang}

# API has trouble with long titles= values.
# To prevent skipping languages, use short slices of 20 titles.
langcount = {}
langs.each_slice(20) do |sublist|
  url = RosettaCode.get_api_url({
    "action" => "query",
    "prop" => "categoryinfo",
    "format" => "xml",
    "titles" => sublist.join("|"),
  })

  doc = REXML::Document.new open(url)
  REXML::XPath.each(doc, "//page") do |page|
    lang = page.attribute("title").value
    info = REXML::XPath.first(page, "categoryinfo")
    langcount[lang] = info.nil? ? 0 : info.attribute("pages").value.to_i
  end
end

puts Time.now
puts "There are #{langcount.length} languages"
puts "the top 25:"
langcount.sort_by {|key,val| val}.reverse[0,25].each_with_index do |(lang, count), i|
  puts "#{i+1}. #{count} - #{lang.sub(/Category:/, '')}"
end

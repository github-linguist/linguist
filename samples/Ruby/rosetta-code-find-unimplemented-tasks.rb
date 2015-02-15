require 'rosettacode'
require 'time'

module RosettaCode
  def self.get_unimplemented(lang)
    programming_tasks = []
    category_members("Programming_Tasks") {|task| programming_tasks << task}

    lang_tasks = []
    category_members(lang) {|task| lang_tasks << task}

    lang_tasks_omit = []
    category_members("#{lang}/Omit") {|task| lang_tasks_omit << task}

    [programming_tasks - lang_tasks, lang_tasks_omit]
  end

  def self.created_time(title)
    url = get_api_url({
      "action" => "query",
      "titles" => title,
      "format" => "xml",
      "rvlimit" => 500,
      "prop" => "revisions",
      "rvprop" => "timestamp"
    })
    doc = REXML::Document.new open(url)
    REXML::XPath.each(doc, "//rev").collect do |node|
      Time.parse( node.attribute("timestamp").value )
    end.min
  end

end

puts Time.now
lang = ARGV[0] || "Ruby"
unimplemented, omitted = RosettaCode.get_unimplemented(lang)
unimplemented.collect {|title| [title, RosettaCode.created_time(title)]} .
              sort_by {|e| e[1]} .
              each do |title, date|
                puts "%s %6s %s" % [
                  date.strftime("%Y-%m-%d"),
                  omitted.include?(title) ? "[omit]" : "" ,
                  title
                ]
              end

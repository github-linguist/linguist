require 'date'
require 'json'
require 'securerandom'

class SimpleDatabase
  def initialize(dbname, *fields)
    @dbname = dbname
    @filename = @dbname + ".dat"
    @fields = fields
    @maxl = @fields.collect {|f| f.length}.max
    @data = {
      'fields' => fields,
      'items' => {},
      'history' => [],
      'tags' => {},
    }
  end
  attr_reader :dbname, :fields

  def self.open(dbname)
    db = new(dbname)
    db.read
    db
  end

  def read()
    if not File.exists?(@filename)
      raise ArgumentError, "Database #@dbname has not been created"
    end
    @data = JSON.parse(File.read(@filename))
    @fields = @data['fields']
    @maxl = @fields.collect {|f| f.length}.max
  end

  def write()
    File.open(@filename, 'w') {|f| f.write(JSON.generate(@data))}
  end

  def add(*values)
    id = SecureRandom.uuid
    @data['items'][id] = Hash[ @fields.zip(values) ]
    @data['history'] << [Time.now.to_f, id]
    id
  end

  def tag(id, *tags)
    tags.each do |tag|
      if @data['tags'][tag].nil?
        @data['tags'][tag] = [id]
      else
        @data['tags'][tag] << id
      end
    end
    id
  end

  def latest
    @data['history'].sort_by {|val| val[0]}.last.last
  end

  def get_item(id)
    @data['items'][id]
  end

  def tags()
    @data['tags'].keys.sort
  end

  def ids_for_tag(tag)
    @data['tags'][tag]
  end

  def tags_for_id(id)
    @data['tags'].keys.inject([]) do |tags, tag|
      tags << tag if @data['tags'][tag].include?(id)
      tags
    end
  end

  def display(id)
    item = get_item(id)
    fmt = "%#{@maxl}s - %s\n"
    puts fmt % ['id', id]
    @fields.each {|f| print fmt % [f, item[f]]}
    puts fmt % ['tags', tags_for_id(id).join(',')]
    added = @data['history'].find {|x| x[1] == id}.first
    puts fmt % ['date added', Time.at(added).ctime]
    puts ""
  end

  def each()
    @data['history'].each {|time, id| yield id}
  end

  def each_item_with_tag(tag)
    @data['tags'][tag].each {|id| yield id}
  end
end
def usage()
  puts <<END
usage: #{$0} command args ...

commands:
  help
  create dbname field ...
  fields dbname
  add dbname value ...
  tag dbname id tag ...
  tags dbname
  list dbname [tag ...]
  latest dbname
  latest_by_tag dbname
END
end

def open_database(args)
  dbname = args.shift
  begin
    SimpleDatabase.open(dbname)
  rescue ArgumentError => e
    STDERR.puts e.message
    exit 1
  end
end

def process_command_line(command, *args)
  case command
  when 'help'
    usage

  when 'create'
    db = SimpleDatabase.new(*args)
    db.write
    puts "Database #{args[0]} created"

  when 'fields'
    db = open_database(args)
    puts "Database #{db.dbname} fields:"
    puts db.fields.join(',')

  when 'add'
    db = open_database(args)
    id = db.add(*args)
    db.write
    puts "Database #{db.dbname} added id #{id}"

  when 'tag'
    db = open_database(args)
    id = args.shift
    db.tag(id, *args)
    db.write
    db.display(id)

  when 'tags'
    db = open_database(args)
    puts "Database #{db.dbname} tags:"
    puts db.tags.join(',')

  when 'list'
    db = open_database(args)
    if args.empty?
      db.each {|id| db.display(id)}
    else
      args.each do |tag|
        puts "Items tagged #{tag}"
        db.each_item_with_tag(tag) {|id| db.display(id)}
      end
    end

  when 'latest'
    db = open_database(args)
    db.display(db.latest)

  when 'latest_by_tag'
    db = open_database(args)
    db.tags.each do |tag|
      puts tag
      db.display(db.ids_for_tag(tag).last)
    end

  else
    puts "Error: unknown command '#{command}'"
    usage
  end
end

process_command_line *ARGV

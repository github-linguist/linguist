notes = 'NOTES.TXT'
if ARGV.empty?
  File.copy_stream(notes, $stdout) rescue nil
else
  File.open(notes, 'a') {|file| file.puts "%s\n\t%s" % [Time.now, ARGV.join(' ')]}
end

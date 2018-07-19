def main
  puts "first instance"
  sleep 20
  puts :done
end

if $0 == __FILE__
  if File.new(__FILE__).flock(File::LOCK_EX | File::LOCK_NB)
    main
  else
    raise "another instance of this program is running"
  end
end

__END__

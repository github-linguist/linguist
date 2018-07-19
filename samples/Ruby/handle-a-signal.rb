t1 = Time.now

catch :done do
  Signal.trap('INT') do
    Signal.trap('INT', 'DEFAULT') # reset to default
    throw :done
  end
  n = 0
  loop do
    sleep(0.5)
    n += 1
    puts n
  end
end

tdelt = Time.now - t1
puts 'Program has run for %5.3f seconds.' % tdelt

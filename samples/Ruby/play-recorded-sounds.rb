require 'win32/sound'
include Win32

sound1 = ENV['WINDIR'] + '\Media\Windows XP Startup.wav'
sound2 = ENV['WINDIR'] + '\Media\Windows XP Shutdown.wav'

puts "play the sounds sequentially"
[sound1, sound2].each do |s|
  t1 = Time.now
  Sound.play(s)
  puts "'#{s}' duration: #{(Time.now.to_f - t1.to_f)} seconds"
end

puts "attempt to play the sounds simultaneously"
[sound1, sound2].each {|s| Sound.play(s, Sound::ASYNC)}

puts <<END
the above only plays the second sound2 because the library only appears
to be able to play one sound at a time.
END

puts "loop a sound for a few seconds"
puts Time.now
Sound.play(sound1, Sound::ASYNC + Sound::LOOP)
sleep 10
Sound.stop
puts Time.now

puts "manipulate the volume"
vol_left, vol_right = Sound.wave_volume
Sound.play(sound1, Sound::ASYNC)
sleep 1
puts "right channel quiet"
Sound.set_wave_volume(vol_left, 0)
sleep 1
puts "left channel quiet"
Sound.set_wave_volume(0, vol_right)
sleep 1
puts "restore volume"
Sound.set_wave_volume(vol_left, vol_right)

sleep 1
puts "the asynchronous sound is cancelled when the program exits"

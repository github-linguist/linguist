def time2deg(t)
  raise "invalid time" unless m = t.match(/^(\d\d):(\d\d):(\d\d)$/)
  hh,mm,ss = m[1..3].map {|e| e.to_i}
  raise "invalid time" unless (0..23).include? hh and
                              (0..59).include? mm and
                              (0..59).include? ss
  (hh*3600 + mm*60 + ss) * 360 / 86400.0
end

def deg2time(d)
  sec = (d % 360) * 86400 / 360.0
  "%02d:%02d:%02d" % [sec/3600, (sec%3600)/60, sec%60]
end

def mean_time(times)
  deg2time(mean_angle(times.map {|t| time2deg t}))
end

puts mean_time ["23:00:17", "23:40:20", "00:12:45", "00:17:19"]

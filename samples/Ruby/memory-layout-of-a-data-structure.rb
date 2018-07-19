require 'bit-struct'

class RS232_9 < BitStruct
  unsigned :cd,  1, "Carrier detect"       #1
  unsigned :rd,  1, "Received data"        #2
  unsigned :td,  1, "Transmitted data"     #3
  unsigned :dtr, 1, "Data terminal ready"  #4
  unsigned :sg,  1, "Signal ground"        #5
  unsigned :dsr, 1, "Data set ready"       #6
  unsigned :rts, 1, "Request to send"      #7
  unsigned :cts, 1, "Clear to send"        #8
  unsigned :ri,  1, "Ring indicator"       #9

  def self.new_with_int(value)
    data = {}
    fields.each_with_index {|f, i| data[f.name] = value[i]}
    new(data)
  end
end

num = rand(2**9 - 1)
puts "num = #{num}"

sample1 = RS232_9.new([("%09d" % num.to_s(2)).reverse].pack("B*"))
puts sample1.inspect_detailed

sample2 = RS232_9.new_with_int(num)
puts sample2.inspect_detailed

puts "CD is #{sample2.cd == 1 ? 'on' : 'off'}"

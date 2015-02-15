text = <<EOS
####        #
#   #       #
#   #       #
####  #  #  ###  #   #
# #   #  #  #  #  # #
#  #  #  #  #  #   #
#   #  ###  ###   #
                 #
                #
EOS

def banner3D(text, shift=-1)
  txt = text.each_line.map{|line| line.gsub('#','__/').gsub(' ','   ')}
  offset = Array.new(txt.size){|i| " " * shift.abs * i}
  offset.reverse! if shift < 0
  txt.each_with_index{|line,i| puts offset[i] + line}
end
banner3D(text)

puts
# Another solution:
def banner3D(text, shift=-2)
  txt = text.each_line.map{|line| line.chomp + ' '}
  offset = Array.new(txt.size){|i| " " * shift.abs * i}
  offset.reverse! if shift < 0
  txt.each_with_index do |line,i|
    line2 = line.gsub!(' ','   ').dup
    puts offset[i] + line.gsub('#','///').gsub('/ ','/\\')
    puts offset[i] + line2.gsub('#','\\\\\\\\\\').gsub('\ ','\/')
  end
end
banner3D(text)

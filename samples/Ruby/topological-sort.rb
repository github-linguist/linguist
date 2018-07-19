require 'tsort'
class Hash
  include TSort
  alias tsort_each_node each_key
  def tsort_each_child(node, &block)
    fetch(node).each(&block)
  end
end

depends = {}
DATA.each do |line|
  libs = line.split(' ')
  key = libs.shift
  depends[key] = libs
  libs.each {|lib| depends[lib] ||= []}
end

begin
  p depends.tsort
  depends["dw01"] << "dw04"
  p depends.tsort
rescue TSort::Cyclic => e
  puts "cycle detected: #{e}"
end

__END__
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys

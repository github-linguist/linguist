def fromRoman(roman)
  r = roman.upcase
  n = 0
  until r.empty? do
    case
    when r.start_with?('M')  then v = 1000; len = 1
    when r.start_with?('CM') then v = 900;  len = 2
    when r.start_with?('D')  then v = 500;  len = 1
    when r.start_with?('CD') then v = 400;  len = 2
    when r.start_with?('C')  then v = 100;  len = 1
    when r.start_with?('XC') then v = 90;   len = 2
    when r.start_with?('L')  then v = 50;   len = 1
    when r.start_with?('XL') then v = 40;   len = 2
    when r.start_with?('X')  then v = 10;   len = 1
    when r.start_with?('IX') then v = 9;    len = 2
    when r.start_with?('V')  then v = 5;    len = 1
    when r.start_with?('IV') then v = 4;    len = 2
    when r.start_with?('I')  then v = 1;    len = 1
    else
      raise ArgumentError.new("invalid roman numerals: " + roman)
    end
    n += v
    r.slice!(0,len)
  end
  n
end

[ "MCMXC", "MMVIII", "MDCLXVI" ].each {|r| p r => fromRoman(r)}

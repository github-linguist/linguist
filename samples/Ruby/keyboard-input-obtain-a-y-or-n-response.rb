def yesno
  begin
    system("stty raw -echo")
    str = STDIN.getc
  ensure
    system("stty -raw echo")
  end
  if str == "Y"
    return true
  elsif str == "N"
    return false
  else
    raise "Invalid character."
  end
end

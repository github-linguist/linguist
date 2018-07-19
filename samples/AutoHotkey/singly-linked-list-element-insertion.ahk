a = 1
a_next = b
b = 2
b_next = 0
c = 3
insert_after("c", "a")
Listvars
msgbox
return

insert_after(new, old)
{
  local temp
  temp := %old%_next
  %old%_next := new
  %new%_next := temp
}

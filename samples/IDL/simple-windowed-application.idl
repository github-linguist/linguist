pro counter, ev
  widget_control, ev.top, get_uvalue=tst
  tst[1] = tst[1]+1
  widget_control, tst[0], set_value="Number of clicks: "+string(tst[1],format='(i0)')
  widget_control, ev.top, set_uvalue=tst
end

id = widget_base(title = 'Window Title',column=1)
ld = widget_label(id, value = 'There have been no clicks yet.')
widget_control, /realize, id, set_uvalue=[ld,0]
dummy = widget_button(id,value=' Click Me ',event_pro='counter')
xmanager, "Simple", Id

end

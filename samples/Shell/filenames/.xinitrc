dwmstatus &
xfce4-screensaver &
st -e irssi &
st -e vim ~/TODO &
chromium &
mbsync -a &
st -e neomutt &
redshift -O3500; xset r rate 300 50; exec startdwm


### startdwm
while true; do
    # Log stderror to a file
    dwm 2> ~/.dwm.log
    # No error logging
    #dwm >/dev/null 2>&1
done


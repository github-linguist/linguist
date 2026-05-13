# bcallah@ ~/.xsession

xset b off &
mixerctl outputs.master=180,180 &
ls ~/.startup/o*.wav | shuf -n 1 | xargs aucat -i &
ls ~/.wallpaper/*.jpg | shuf -n 1 | xargs feh --bg-fill --no-fehbg &
xcompmgr -Ff &
xrdb ~/.Xresources &
tint2 &
exec cwm

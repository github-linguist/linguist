# example taken from https://github.com/ycollet/fedora-spec/blob/master/kickstart/fedora-32-live-jam-xfce-pipewire.ks
# Lic. GPL-3.0

# fedora-livedvd-jam-xfe.ks
# With XFCE Desktop

# Fedora Jam: For Musicians and audio enthusiasts
# Fedora Jam is a spin for anyone interested in creating 
# music 

# Maintainer: Yann Collette <ycollette.nospam@free.fr>

lang fr_FR.UTF-8
keyboard fr-latin9
timezone Europe/Paris

auth --useshadow --passalgo=sha512
# SELinux configuration
selinux --enforcing
firewall --enabled --service=mdns
xconfig --startxonboot
# Clear the Master Boot Record
zerombr
clearpart --all --initlabel
part / --size 16384 --fstype ext4
services --disabled="sshd" --enabled="NetworkManager"
network --bootproto=dhcp --device=link --activate
# Shutdown after installation
shutdown
rootpw --plaintext audinux

#enable threaded irqs
bootloader --location=none --append="threadirqs nopti"

repo --name=rpmfusion                --baseurl=http://download1.rpmfusion.org/free/fedora/releases/$releasever/Everything/$basearch/os/
repo --name=rpmfusion-non-free       --baseurl=http://download1.rpmfusion.org/nonfree/fedora/releases/$releasever/Everything/$basearch/os/
repo --name=rpmfusion-update-testing --baseurl=http://download1.rpmfusion.org/free/fedora/updates/testing/$releasever/$basearch/
repo --name=rpmfusion-free-update    --baseurl=http://download1.rpmfusion.org/free/fedora/updates/$releasever/$basearch/

repo --name=fedora  --mirrorlist=http://mirrors.fedoraproject.org/metalink?repo=fedora-$releasever&arch=$basearch
repo --name=updates --mirrorlist=http://mirrors.fedoraproject.org/metalink?repo=updates-released-f$releasever&arch=$basearch

url --mirrorlist=https://mirrors.fedoraproject.org/metalink?repo=fedora-$releasever&arch=$basearch

repo --name="CoprLinuxMAO" --baseurl=https://copr-be.cloud.fedoraproject.org/results/ycollet/linuxmao/fedora-$releasever-$basearch/

%packages

@base-x
@guest-desktop-agents
@standard
@core
@fonts
@input-methods
@multimedia
@hardware-support
dnf

# exclude input methods:
-scim*
-ibus*
-iok

# Make live images easy to shutdown and the like in libvirt
qemu-guest-agent

%end

%post

# Install language pack
dnf -y langinstall French

# FIXME: it'd be better to get this installed from a package
cat > /etc/rc.d/init.d/livesys << EOF
#!/bin/bash
#
# live: Init script for live image
#
# chkconfig: 345 00 99
# description: Init script for live image.
### BEGIN INIT INFO
# X-Start-Before: display-manager chronyd
### END INIT INFO

. /etc/init.d/functions

if ! strstr "\`cat /proc/cmdline\`" rd.live.image || [ "\$1" != "start" ]; then
    exit 0
fi

if [ -e /.liveimg-configured ] ; then
    configdone=1
fi

exists() {
    which \$1 >/dev/null 2>&1 || return
    \$*
}

# Make sure we don't mangle the hardware clock on shutdown
ln -sf /dev/null /etc/systemd/system/hwclock-save.service

livedir="LiveOS"
for arg in \`cat /proc/cmdline\` ; do
  if [ "\${arg##rd.live.dir=}" != "\${arg}" ]; then
    livedir=\${arg##rd.live.dir=}
    continue
  fi
  if [ "\${arg##live_dir=}" != "\${arg}" ]; then
    livedir=\${arg##live_dir=}
  fi
done

# enable swaps unless requested otherwise
swaps=\`blkid -t TYPE=swap -o device\`
if ! strstr "\`cat /proc/cmdline\`" noswap && [ -n "\$swaps" ] ; then
  for s in \$swaps ; do
    action "Enabling swap partition \$s" swapon \$s
  done
fi
if ! strstr "\`cat /proc/cmdline\`" noswap && [ -f /run/initramfs/live/\${livedir}/swap.img ] ; then
  action "Enabling swap file" swapon /run/initramfs/live/\${livedir}/swap.img
fi

mountPersistentHome() {
  # support label/uuid
  if [ "\${homedev##LABEL=}" != "\${homedev}" -o "\${homedev##UUID=}" != "\${homedev}" ]; then
    homedev=\`/sbin/blkid -o device -t "\$homedev"\`
  fi

  # if we're given a file rather than a blockdev, loopback it
  if [ "\${homedev##mtd}" != "\${homedev}" ]; then
    # mtd devs don't have a block device but get magic-mounted with -t jffs2
    mountopts="-t jffs2"
  elif [ ! -b "\$homedev" ]; then
    loopdev=\`losetup -f\`
    if [ "\${homedev##/run/initramfs/live}" != "\${homedev}" ]; then
      action "Remounting live store r/w" mount -o remount,rw /run/initramfs/live
    fi
    losetup \$loopdev \$homedev
    homedev=\$loopdev
  fi

  # if it's encrypted, we need to unlock it
  if [ "\$(/sbin/blkid -s TYPE -o value \$homedev 2>/dev/null)" = "crypto_LUKS" ]; then
    echo
    echo "Setting up encrypted /home device"
    plymouth ask-for-password --command="cryptsetup luksOpen \$homedev EncHome"
    homedev=/dev/mapper/EncHome
  fi

  # and finally do the mount
  mount \$mountopts \$homedev /home
  # if we have /home under what's passed for persistent home, then
  # we should make that the real /home.  useful for mtd device on olpc
  if [ -d /home/home ]; then mount --bind /home/home /home ; fi
  [ -x /sbin/restorecon ] && /sbin/restorecon /home
  if [ -d /home/audinux ]; then USERADDARGS="-M" ; fi
}

findPersistentHome() {
  for arg in \`cat /proc/cmdline\` ; do
    if [ "\${arg##persistenthome=}" != "\${arg}" ]; then
      homedev=\${arg##persistenthome=}
    fi
  done
}

if strstr "\`cat /proc/cmdline\`" persistenthome= ; then
  findPersistentHome
elif [ -e /run/initramfs/live/\${livedir}/home.img ]; then
  homedev=/run/initramfs/live/\${livedir}/home.img
fi

# if we have a persistent /home, then we want to go ahead and mount it
if ! strstr "\`cat /proc/cmdline\`" nopersistenthome && [ -n "\$homedev" ] ; then
  action "Mounting persistent /home" mountPersistentHome
fi

if [ -n "\$configdone" ]; then
  exit 0
fi

# add fedora user with no passwd
action "Adding live user" useradd \$USERADDARGS -c "Live System User" audinux
passwd -d audinux > /dev/null
usermod -aG wheel    audinux > /dev/null
usermod -aG jackuser audinux > /dev/null
usermod -aG root     audinux > /dev/null

# Remove root password lock
passwd -d root > /dev/null

# turn off firstboot for livecd boots
systemctl --no-reload disable firstboot-text.service 2> /dev/null || :
systemctl --no-reload disable firstboot-graphical.service 2> /dev/null || :
systemctl stop firstboot-text.service 2> /dev/null || :
systemctl stop firstboot-graphical.service 2> /dev/null || :

# don't use prelink on a running live image
sed -i 's/PRELINKING=yes/PRELINKING=no/' /etc/sysconfig/prelink &>/dev/null || :

# turn off mdmonitor by default
systemctl --no-reload disable mdmonitor.service 2> /dev/null || :
systemctl --no-reload disable mdmonitor-takeover.service 2> /dev/null || :
systemctl stop mdmonitor.service 2> /dev/null || :
systemctl stop mdmonitor-takeover.service 2> /dev/null || :

# don't enable the gnome-settings-daemon packagekit plugin
gsettings set org.gnome.software download-updates 'false' || :

# don't start cron/at as they tend to spawn things which are
# disk intensive that are painful on a live image
systemctl --no-reload disable crond.service 2> /dev/null || :
systemctl --no-reload disable atd.service 2> /dev/null || :
systemctl stop crond.service 2> /dev/null || :
systemctl stop atd.service 2> /dev/null || :

# turn off abrtd on a live image
systemctl --no-reload disable abrtd.service 2> /dev/null || :
systemctl stop abrtd.service 2> /dev/null || :

# Don't sync the system clock when running live (RHBZ #1018162)
sed -i 's/rtcsync//' /etc/chrony.conf

# Mark things as configured
touch /.liveimg-configured

# add static hostname to work around xauth bug
# https://bugzilla.redhat.com/show_bug.cgi?id=679486
# the hostname must be something else than 'localhost'
# https://bugzilla.redhat.com/show_bug.cgi?id=1370222
echo "localhost-live" > /etc/hostname

EOF

# bah, hal starts way too late
cat > /etc/rc.d/init.d/livesys-late << EOF
#!/bin/bash
#
# live: Late init script for live image
#
# chkconfig: 345 99 01
# description: Late init script for live image.

. /etc/init.d/functions

if ! strstr "\`cat /proc/cmdline\`" rd.live.image || [ "\$1" != "start" ] || [ -e /.liveimg-late-configured ] ; then
    exit 0
fi

exists() {
    which \$1 >/dev/null 2>&1 || return
    \$*
}

touch /.liveimg-late-configured

# read some variables out of /proc/cmdline
for o in \`cat /proc/cmdline\` ; do
    case \$o in
    ks=*)
        ks="--kickstart=\${o#ks=}"
        ;;
    xdriver=*)
        xdriver="\${o#xdriver=}"
        ;;
    esac
done

# if liveinst or textinst is given, start anaconda
if strstr "\`cat /proc/cmdline\`" liveinst ; then
   plymouth --quit
   /usr/sbin/liveinst \$ks
fi
if strstr "\`cat /proc/cmdline\`" textinst ; then
   plymouth --quit
   /usr/sbin/liveinst --text \$ks
fi

# configure X, allowing user to override xdriver
cat > /etc/X11/xorg.conf <<FOE
Section "InputDevice"
    Identifier "Keyboard0"
    Driver "kbd"
    Option "XkbLayout" "fr-latin9"
EndSection
FOE

if [ -n "\$xdriver" ]; then
   cat > /etc/X11/xorg.conf.d/00-xdriver.conf <<FOE
Section "Device"
    Identifier "Videocard0"
    Driver "\$xdriver"
EndSection
FOE
fi

EOF

chmod 755 /etc/rc.d/init.d/livesys
/sbin/restorecon /etc/rc.d/init.d/livesys
/sbin/chkconfig --add livesys

chmod 755 /etc/rc.d/init.d/livesys-late
/sbin/restorecon /etc/rc.d/init.d/livesys-late
/sbin/chkconfig --add livesys-late

# enable tmpfs for /tmp
systemctl enable tmp.mount

# make it so that we don't do writing to the overlay for things which
# are just tmpdirs/caches
# note https://bugzilla.redhat.com/show_bug.cgi?id=1135475
cat >> /etc/fstab << EOF
vartmp   /var/tmp    tmpfs   defaults   0  0
EOF

# work around for poor key import UI in PackageKit
rm -f /var/lib/rpm/__db*
releasever=$(rpm -q --qf '%{version}\n' --whatprovides system-release)
basearch=$(uname -i)
rpm --import /etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-$releasever-$basearch
echo "Packages within this LiveCD"
rpm -qa
# Note that running rpm recreates the rpm db files which aren't needed or wanted
rm -f /var/lib/rpm/__db*

# go ahead and pre-make the man -k cache (#455968)
/usr/bin/mandb

# make sure there aren't core files lying around
rm -f /core*

# remove random seed, the newly installed instance should make it's own
rm -f /var/lib/systemd/random-seed

# convince readahead not to collect
# FIXME: for systemd

# forcibly regenerate fontconfig cache (so long as this live image has
# fontconfig) - see #1169979
if [ -x /usr/bin/fc-cache ] ; then
   fc-cache -f
fi

echo 'File created by kickstart. See systemd-update-done.service(8).' \
    | tee /etc/.updated >/var/.updated

# Drop the rescue kernel and initramfs, we don't need them on the live media itself.
# See bug 1317709
rm -f /boot/*-rescue*

# Disable network service here, as doing it in the services line
# fails due to RHBZ #1369794
/sbin/chkconfig network off

# Remove machine-id on pre generated images
rm -f /etc/machine-id
touch /etc/machine-id

%end

%post --nochroot
#YC: no such files in F31 - cp $INSTALL_ROOT/usr/share/licenses/*-release/* $LIVE_ROOT/

# only works on x86, x86_64
if [ "$(uname -i)" = "i386" -o "$(uname -i)" = "x86_64" ]; then
    # For livecd-creator builds
    if [ ! -d $LIVE_ROOT/LiveOS ]; then mkdir -p $LIVE_ROOT/LiveOS ; fi
    cp /usr/bin/livecd-iso-to-disk $LIVE_ROOT/LiveOS

    # For lorax/livemedia-creator builds
    sed -i '
    /## make boot.iso/ i\
    # Add livecd-iso-to-disk script to .iso filesystem at /LiveOS/\
    <% f = "usr/bin/livecd-iso-to-disk" %>\
    %if exists(f):\
        install ${f} ${LIVEDIR}/${f|basename}\
    %endif\
    ' /usr/share/lorax/templates.d/99-generic/live/x86.tmpl
fi

%end

#################################
# List packages to be installed #
#################################

%packages

# system packages

# Explicitly specified here:
# <notting> walters: because otherwise dependency loops cause yum issues.
kernel
kernel-modules
kernel-modules-extra
kernel-tools
# kernel-rt-mao
# YC: livecd-creator doesn't manage kernel-rt-* naming, only kernel-*

# This was added a while ago, I think it falls into the category of
# "Diagnosis/recovery tool useful from a Live OS image".  Leaving this untouched
# for now.
memtest86+

syslinux

# Without this, initramfs generation during live image creation fails: #1242586
dracut-live
grub2
grub2-tools
grub2-efi
#shim
#shim-unsigned

# save some space
-mpage
-sox
-hplip
-numactl
-isdn4k-utils
-autofs
# smartcards won't really work on the livecd.
-coolkey
-wget

# scanning takes quite a bit of space :/
-xsane
-xsane-gimp
-sane-backends

# XFCE
@xfce-desktop
@xfce-apps
@xfce-extra-plugins
@xfce-media
@xfce-office

# unlock default keyring. FIXME: Should probably be done in comps
gnome-keyring-pam
fedora-release

# various system package (since F31)
chkconfig

# save some space
-autofs
-acpid
-gimp-help
-desktop-backgrounds-basic
-realmd                     # only seems to be used in GNOME
-aspell-*                   # dictionaries are big
-gnumeric
-foomatic-db-ppds
-foomatic
-stix-fonts
-ibus-typing-booster
-xfce4-sensors-plugin

# drop some system-config things
#-system-config-boot
#-system-config-network
-system-config-rootpassword
#-system-config-services
-policycoreutils-gui
#-libcrypt-nss
python3

# alsa
alsa-firmware
alsa-tools
alsa-utils
alsamixergui
alsa-plugins-jack
alsa-plugins-pulseaudio
alsa-plugins-usbstream
alsa-plugins-samplerate
alsa-plugins-upmix
alsa-plugins-vdownmix
a2jmidid
#YC aj-snapshot

# jack 
pipewire-jack-audio-connection-kit
pipewire-alsa
pipewire-gstreamer
pipewire-libjack
qjackctl
jackctlmmc
Cadence

# ffado
ffado

# pulse
pavucontrol

# midi
qsynth
fluidsynth
fluid-soundfont-gm
fluidsynth-dssi
timidity++
qmidiarp
vmpk
mamba

# synthesis
hydrogen
#YC hydrogen-drumkit-AVL-BlackPearl
bristol
yoshimi
zynaddsubfx
swami
synthv1
samplv1
ams
aeolus
minicomputer
phasex
Rack-v1
rack-v1-Befaco
rack-v1-ESeries
rack-v1-AudibleInstruments
rack-v1-Fundamental

# guitar
guitarix
tuxguitar
sooperlooper

# recodring and DAW
audacity
ardour5
ardour5-audiobackend-alsa
ardour5-audiobackend-jack
ardour5-audiobackend-dummy
#YC seq24
qtractor
non-daw
non-mixer
non-sequencer
non-session-manager
muse
rosegarden4
mixxx
milkytracker

# audio-plugins
calf
dssi
ladspa
helm
DISTRHO-Ports
6PM
synthpod

# ladpsa plugins
ladspa-calf-plugins
ladspa-caps-plugins
ladspa-amb-plugins
ladspa-autotalent-plugins
ladspa-blop-plugins
ladspa-cmt-plugins
ladspa-fil-plugins
ladspa-mcp-plugins
ladspa-rev-plugins
ladspa-swh-plugins
ladspa-tap-plugins
ladspa-vco-plugins
#YC ladspa-vocoder-plugins
ladspa-wasp-plugins

# lv2 plugins
lv2
#YC lv2-avw-plugins
#YC pyliblo missing lv2-fil-plugins
lv2-invada-plugins
#YC lv2-kn0ck0ut
lv2-ll-plugins
swh-lv2
lv2-vocoder-plugins
lv2-zynadd-plugins
lv2dynparam
lv2-abGate
lv2-c++-tools 
lv2-samplv1
lv2-synthv1
lv2-drumkv1
#YC lv2-triceratops
lv2-newtonator
lv2-x42-plugins
#YC lv2-fomp-plugins
lv2-sorcer
lv2-fabla
lv2-artyfx-plugins
lv2-EQ10Q-plugins
lv2-linuxsampler-plugins
lv2-mdaEPiano
lv2-mdala-plugins
swh-lv2
orbit.lv2
midi_matrix.lv2
sherlock.lv2
eteroj.lv2
zam-plugins
vopa-lv2
tap-lv2
#sisco.lv2
mda-lv2
rkrlv2
ams-lv2

# dssi
nekobee-dssi
whysynth-dssi
xsynth-dssi
hexter-dssi

# VST3
vst3-surge

# Zita tools
zita-at1
zita-rev1
zita-ajbridge
zita-alsa-pcmi
zita-convolver
zita-lrx
zita-njbridge
zita-resampler

# writing & publishing
vim
nano
mscore
lilypond

# audio utilities
jamin
lash
jack_capture
jaaa
jmeters
qastools
arpage
realTimeConfigQuickScan
rtirq
japa
radium-compressor
#YC solfege
linuxsampler
# qsampler # YC libgig missing ...
projectM-jack
projectM-pulseaudio

protrekkr
oxefmsynth
stretchplayer
sfarkxtc
lenmus
GrandOrgue
picoloop
jm2cv
rakarrack

#language
chuck
miniaudicle
supercollider
supercollider-sc3-plugins
supercollider-vim
lmms-mao
faust
faust-tools
faustworks

# fedora jam theming (to be customized)
fedora-jam-backgrounds

# Misc. Utils
screen
multimedia-menus
xterm
emacs

# Include Mozilla Firefox and Thunderbird
firefox
thunderbird

##########
# Remove #
##########

## These are packages that are pulled for one reason or another but are safe to remove.
-@input-methods              ## Not necessary can be installed later.
-@dial-up                    ## Not even old computers use dialup anymore.
-system-config-firewall-base ## Doesn't seem to do anything
-gfs2-utils                  ## Part of kernel debug
-kernel-debug-modules-extra  ## Part of kernel debug
-kernel-debug                ## Dont need the debug kernel upon install
-aspell-*                    ## Dictionaries are big and take up space
-man-pages-*                 ## Dictionaries
-words                       ## Dictionaries
-krb5-auth-dialog            ## Legacy and cmdline things we don't want
-krb5-workstation            ## Legacy
-pam_krb5                    ## Legacy
-quota                       ## Legacy
-minicom                     ## Legacy
-dos2unix                    ## Legacy
-finger                      ## Legacy
-ftp                         ## Legacy
-jwhois                      ## Legacy
-mtr                         ## Legacy
-pinfo                       ## Legacy
-rsh                         ## Legacy
-telnet                      ## Legacy
-nfs-utils                   ## Legacy
-ypbind                      ## Legacy
-yp-tools                    ## Legacy
-rpcbind                     ## Legacy
-acpid                       ## Legacy
-ntsysv                      ## Legacy
-rmt                         ## Legacy
-talk                        ## Legacy
-lftp                        ## Legacy
-tcpdump                     ## Legacy
-dump                        ## Legacy
-@printing                   ## We don't want printer support out of the box.
-fprintd-pam                 ## We don't want printer support out of the box.
-fprintd                     ## We don't want printer support out of the box.
-libfprint                   ## We don't want printer support out of the box.
-python3-cups                ## We don't want printer support out of the box.
-system-config-printer-libs  ## We don't want printer support out of the box.
-ibus-typing-booster         ## Tab completion in libreoffice and the likes Unneeded
-libtranslit                 ## Tab
-libtranslit-m17n            ## Tab

# Not really useful
# -fedora-jam-backgrounds-kde
# FC28 required now -tigervnc-server-minimal
-abiword
-xfburn
-lyx-fonts
-goffice
-midori

%end

%post --nochroot

mkdir -p $INSTALL_ROOT/home/audinux/SoundFonts
mkdir -p $INSTALL_ROOT/home/audinux/GuitarPro

cp -r /tmp/prepare/audinux/SoundFonts/*   $INSTALL_ROOT/home/audinux/SoundFonts/
cp -r /tmp/prepare/audinux/GuitarPro/*    $INSTALL_ROOT/home/audinux/GuitarPro/
cp /tmp/prepare/audinux/Images/skulls.jpg $INSTALL_ROOT/usr/share/backgrounds/images/

%end

%post

if [ ! -d /home/audinux/Desktop ]; then
  mkdir -p /home/audinux/Desktop
fi

# Rewrite limits.conf for jack use
cat > /etc/security/limits.d/95-jack.conf <<EOF
# Default limits for users of jack-audio-connection-kit

@jackuser - rtprio 90
@jackuser - nice -10
@jackuser - memlock unlimited
EOF

# xfce configuration

# This is a huge file and things work ok without it
rm -f /usr/share/icons/HighContrast/icon-theme.cache

# create /etc/sysconfig/desktop (needed for installation)

cat > /etc/sysconfig/desktop <<EOF
PREFERRED=/usr/bin/startxfce4
DISPLAYMANAGER=/usr/sbin/lightdm
EOF

# add initscript
cat >> /etc/rc.d/init.d/livesys << EOF

mkdir -p /home/audinux/.config/xfce4

cat > /home/audinux/.config/xfce4/helpers.rc << FOE
MailReader=thunderbird
FileManager=Thunar
WebBrowser=firefox
FOE

# disable screensaver locking (#674410)
cat >> /home/audinux/.xscreensaver << FOE
mode:           off
lock:           False
dpmsEnabled:    False
FOE

# deactivate xfconf-migration (#683161)
rm -f /etc/xdg/autostart/xfconf-migration-4.6.desktop || :

# deactivate xfce4-panel first-run dialog (#693569)
mkdir -p /home/audinux/.config/xfce4/xfconf/xfce-perchannel-xml
cp /etc/xdg/xfce4/panel/default.xml /home/audinux/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml

# Set french keyboard for xfce
cat >> /home/audinux/.config/xfce4/xfconf/xfce-perchannel-xml/keyboard-layout.xml << FOE
<?xml version="1.0" encoding="UTF-8"?>

<channel name="keyboard-layout" version="1.0">
  <property name="Default"      type="empty">
    <property name="XkbDisable" type="bool"   value="false"/>
    <property name="XkbLayout"  type="string" value="fr"/>
    <property name="XkbVariant" type="string" value=""/>
  </property>
</channel>
FOE

# Set the background image for the main desktop
# <property name="image-path"        type="string" value="/usr/share/backgrounds/xfce/xfce-blue.jpg"/>
# <property name="last-single-image" type="string" value="/usr/share/backgrounds/xfce/xfce-blue.jpg"/>

cat >> /home/audinux/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-desktop.xml << FOE
<?xml version="1.0" encoding="UTF-8"?>

<channel name="xfce4-desktop" version="1.0">
  <property name="backdrop" type="empty">
    <property name="screen0" type="empty">
      <property name="monitor0" type="empty">skulls.jpg
        <property name="image-path"        type="string" value="/usr/share/backgrounds/images/skulls.jpg"/>
        <property name="last-image"        type="string" value="/usr/share/backgrounds/images/default.png"/>
        <property name="last-single-image" type="string" value="/usr/share/backgrounds/images/skulls.jpg"/>
      </property>
    </property>
  </property>
</channel>
FOE

# set up lightdm autologin
sed -i 's/^#autologin-user=.*/autologin-user=audinux/' /etc/lightdm/lightdm.conf
sed -i 's/^#autologin-user-timeout=.*/autologin-user-timeout=0/' /etc/lightdm/lightdm.conf
#sed -i 's/^#show-language-selector=.*/show-language-selector=true/' /etc/lightdm/lightdm-gtk-greeter.conf

# set Xfce as default session, otherwise login will fail
sed -i 's/^#user-session=.*/user-session=xfce/' /etc/lightdm/lightdm.conf

# Show harddisk install on the desktop
sed -i -e 's/NoDisplay=true/NoDisplay=false/' /usr/share/applications/liveinst.desktop

if [ ! -d /home/audinux/Desktop ]; then
  mkdir /home/audinux/Desktop
fi

cp /usr/share/applications/liveinst.desktop /home/audinux/Desktop

# and mark it as executable (new Xfce security feature)
chmod +x /home/audinux/Desktop/liveinst.desktop

# this goes at the end after all other changes. 
chown -R audinux:audinux /home/audinux
restorecon -R /home/audinux

EOF

# Install applications on the Desktop

cat >> /etc/rc.d/init.d/livesys << EOF

# Copy some applications on desktop
cp /usr/share/applications/qjackctl.desktop       /home/audinux/Desktop
cp /usr/share/applications/guitarix.desktop       /home/audinux/Desktop
cp /usr/share/applications/qsynth.desktop         /home/audinux/Desktop
cp /usr/share/applications/yoshimi.desktop        /home/audinux/Desktop
cp /usr/share/applications/sooperlooper.desktop   /home/audinux/Desktop
cp /usr/share/applications/lmms.desktop           /home/audinux/Desktop
cp /usr/share/applications/mscore.desktop         /home/audinux/Desktop
cp /usr/share/applications/qtractor.desktop       /home/audinux/Desktop
cp /usr/share/applications/audacity.desktop       /home/audinux/Desktop
cp /usr/share/applications/tuxguitar.desktop      /home/audinux/Desktop
cp /usr/share/applications/xfce4-terminal.desktop /home/audinux/Desktop
cp /usr/share/applications/carla.desktop          /home/audinux/Desktop
cp /usr/share/applications/cadence.desktop        /home/audinux/Desktop
cp /usr/share/applications/ardour4.desktop        /home/audinux/Desktop

chmod +x /home/audinux/Desktop/*.desktop

# Disable plasma-pk-updates (bz #1436873 and 1206760)
echo "Removing plasma-pk-updates package."
rpm -e plasma-pk-updates

# Disable baloo
cat > /home/audinux/.config/baloofilerc << BALOO_EOF
[Basic Settings]
Indexing-Enabled=false
BALOO_EOF

# Disable kres-migrator
cat > /home/audinux/.kde/share/config/kres-migratorrc << KRES_EOF
[Migration]
Enabled=false
KRES_EOF

# Disable kwallet migrator
cat > /home/audinux/.config/kwalletrc << KWALLET_EOL
[Migration]
alreadyMigrated=true
KWALLET_EOL

# make sure to set the right permissions and selinux contexts
setfiles -v /etc/selinux/strict/contexts/files/file_contexts /
chown -R audinux:audinux /home/audinux/
restorecon -R /home/audinux/

EOF

%end

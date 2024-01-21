# example taken from https://github.com/xiaomatech/cobbler/blob/master/kickstarts/mysql.ks
# Lic. MIT

# kickstart template for Fedora 8 and later.
# (includes %end blocks)
# do not use with earlier distros

#platform=x86, AMD64, or Intel EM64T
# System authorization information
auth  --useshadow  --enablemd5
# System bootloader configuration
bootloader --location=mbr
# Partition clearing information
clearpart --all --initlabel
# Use text mode install
text
# Firewall configuration
firewall --disabled
# Run the Setup Agent on first boot
firstboot --disable
# System keyboard
keyboard us
# System language
lang en_US.UTF-8
# Use network installation
url --url=$tree
# If any cobbler repo definitions were referenced in the kickstart profile, include them here.
$yum_repo_stanza
# Network information
$SNIPPET('network_config')
# Reboot after installation
reboot

#Root password
rootpw --iscrypted $default_password_crypted
# SELinux configuration
selinux --disabled
# Do not configure the X Window System
skipx
# System timezone
timezone  Asia/Chongqing
# Install OS instead of upgrade
install
# Clear the Master Boot Record
zerombr
# Allow anaconda to partition the system as needed
%include /tmp/partition.ks

#disable some service and enable some service
$SNIPPET('services')

%pre
$SNIPPET('log_ks_pre')
$SNIPPET('kickstart_start')
$SNIPPET('pre_install_network_config')
# Enable installation monitoring
$SNIPPET('pre_anamon')
$SNIPPET('pre_partition')
%end

#%packages
%packages
$SNIPPET('pre_packages')
%end

%post
$SNIPPET('log_ks_post')
# Start yum configuration
$yum_config_stanza
# End yum configuration
$SNIPPET('post_sync_time')
$SNIPPET('post_install_kernel_options')
$SNIPPET('post_install_network_config')
$SNIPPET('download_config_files')
$SNIPPET('cobbler_register')
# Enable post-install boot notification
$SNIPPET('post_anamon')

#### repo setup ###
$SNIPPET('post_repo_config')

#clean root directory
$SNIPPET('post_clean_dir')

### Sync Time ###
$SNIPPET('post_sync_time')

### post util ###
$SNIPPET('post_util')

#mysql
$SNIPPET('post_install_mysql')

#affinity
$SNIPPET('set_affinity')

# Start final steps
$SNIPPET('publickey_root_robin')
$SNIPPET('kickstart_done')
# End final steps
%end

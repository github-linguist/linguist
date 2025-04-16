unique template site/one/onevm;

include 'components/chkconfig/config';

# set opennebula map
include 'quattor/aii/opennebula/schema';
bind "/system/opennebula" = opennebula_vmtemplate;

include 'site/config-vm';

include 'quattor/aii/opennebula/default';

"/software/packages/{acpid}" = dict();
"/software/components/chkconfig/service/acpid" = dict('on', '', 'startstop', true);

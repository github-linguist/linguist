unique template site/ceph/server/infernalis;

include 'components/dirperm/config';

"/software/components/dirperm/paths" = {
    foreach (idx; mp; value('/system/filesystems')) {
        if (match(mp['mountpoint'], format('^%s', CEPH_OSD_MP_BASE))) {
            append(SELF, dict(
                "path", mp['mountpoint'],
                "owner", "ceph:ceph",
                "perm", "0755",
                "type", "d",
            ));
        };
    };
    SELF;
};

include 'common/sysctl/service';
prefix "/software/components/metaconfig/services/{/etc/sysctl.conf}/contents";

'kernel.pid_max' = 4194303;

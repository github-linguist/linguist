unique template site/filesystems/ceph-raid;

prefix '/system/blockdevices';

variable CEPH_OSD_DISKS = {
    # SAS disks partitions
    disks = list();
    foreach (disk; data; value('/hardware/harddisks')) {
        if (data['capacity'] > 1000 * GB) {
            append(disks, disk);
        };
    };
    disks;
};

'partitions' = {
    foreach (idx; disk; CEPH_OSD_DISKS) {
        partitions_add(
            disk, dict(
                format('%s1', disk), 10 * GB,
                format('%s2', disk), 5 * GB,
                format('%s3', disk), -1));
        SELF[format('%s1', disk)]['offset'] = 1;
    };
    SELF;
};

#raid for data
'md' = {
    for (i = 0; i < length(CEPH_OSD_DISKS); i = i + 2) {
        for (j = 2; j <= 3; j = j + 1) {
            SELF[escape(format('md/%s0%s%d', CEPH_OSD_DISKS[i], CEPH_OSD_DISKS[i+1], j ))] = dict(
                "device_list", list(format('partitions/%s%d', CEPH_OSD_DISKS[i], j), format('partitions/%s%d', CEPH_OSD_DISKS[i+1], j)),
                "raid_level", 'RAID0',
                "metadata", '1.2',
            );
        };
    };
    SELF;
};

# ceph OSD and journal fs
'/system/filesystems' = {
    # ga over software raids..
    foreach (disk; data; value('/system/blockdevices/md')) { #check for data part/disk
        if (match(unescape(disk), '^md/.+0.+3$')) {
            append(merge(CEPH_FSOPTS_BASE, CEPH_DISK_OPTIONS[CEPH_FS], dict(
                'mountpoint', format('/var/lib/ceph/osd/%s', replace('md/([a-z0A-Z]+)[0-9]*$', '$1', unescape(disk))),
                'block_device', format('md/%s', disk),
            )));
        } else if (match(unescape(disk), '^md/.+0.+2$')) {
            append(merge(CEPH_FSOPTS_DUMMY, dict(
                'mountpoint', format('/dummy/%s', unescape(disk)),
                'block_device', format('md/%s', disk)
            )));
        };
    };
    SELF;
};

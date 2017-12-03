unique template site/ceph/osdschemas/osd-fetch;

prefix '/software/components/ceph/clusters/ceph';

variable FETCHED_OSDS = {
    t = dict();
    rep = 2;
    foreach(idx; host; CEPH_NODES) {
        prof = replace('.data$', '.os', host);
        d = value(format('%s:/software/components/ceph/localdaemons/osds', prof));
        t[shorten_fqdn(host)] = dict(
            'fqdn', host,
            'osds', d
        );

        numosd = length(d);
        if (numosd > rep){
            rep = numosd;
        };
    };
    all = dict('osdhosts', t, 'maxosd', rep);
};

'osdhosts' = FETCHED_OSDS['osdhosts'];
variable CEPH_OSD_DOWN_REPORTERS ?= FETCHED_OSDS['maxosd'] + 2;
variable CEPH_OSD_DOWN_REPORTS ?=  CEPH_OSD_DOWN_REPORTERS + CEPH_OSD_DOWN_REPORTERS / 4 + 1;

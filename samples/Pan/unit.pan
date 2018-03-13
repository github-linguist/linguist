unique template site/dcache/unit;

include 'components/dcache/config';

## unit/ugroups
## list of ugroups that will be ignored during configuration
"/software/components/dcache/unit/ignore_ugroup" = list();
"/software/components/dcache/unit/units" = dict(
    "protocol", list(
        dict("cond", "*/*", "ugroup", list("default_protocol"))
    ),
    "net", list(
        dict("cond", "192.168.0.0/255.255.0.0", "ugroup", list("in_net", "all_net")),
        dict("cond", "192.168.10.0/255.255.255.0", "ugroup", list("in_server", "in_net", "all_net")),
        dict("cond", "192.168.11.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.12.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.13.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.14.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.15.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.16.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "192.168.17.0/255.255.255.0", "ugroup", list("in_wn", "in_net", "all_net")),
        dict("cond", "0.0.0.0/0.0.0.0", "ugroup", list("all_net")),
        ),
    "store", list(
        dict("cond", "*@*", "ugroup", list("any_store")),
        dict("cond", "myStore:STRING@osm", "ugroup", list("default_store", "any_store")),
        dict("cond", "dteam:dteam-base@osm", "ugroup", list("dteam_store", "any_store")),
        dict("cond", "ops:ops-base@osm", "ugroup", list("ops_store", "any_store")),
        dict("cond", "cms:cms-base@osm", "ugroup", list("cms_store", "any_store")),
        dict("cond", "test:cms-test@osm", "ugroup", list("test_store")),
    ),
);

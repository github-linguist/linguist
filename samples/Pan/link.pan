unique template site/dcache/link;

include 'components/dcache/config';

## links
## default preference value
"/software/components/dcache/link/def_pref" = "10";
## list of links that will be ignored during configuration
"/software/components/dcache/link/ignore_link" = list();
##
"/software/components/dcache/link/links" = dict(
    ## out_buf_write: all outside to write to the storage through this buffer
    "out", dict("ugroup", list("all_net", "any_store"), "pgroup", list("out_buf"), "read", "10", "write", "10", "cache", "10"),
    "in", dict("ugroup", list("in_net", "any_store"), "pgroup", list("priv"), "read", "20", "write", "20", "cache", "20"),
    "dteam", dict("ugroup", list("dteam_store"), "pgroup", list("out_buf"), "read", "10", "write", "10", "cache", "10"),
    "ops", dict("ugroup", list("ops_store"), "pgroup", list("out_buf"), "read", "10", "write", "10", "cache", "10"),
    "cms", dict("ugroup", list("cms_store"), "pgroup", list("out_buf"), "read", "10", "write", "10", "cache", "10"),
    "test", dict("ugroup", list("test_store"), "pgroup", list("behar_test"), "read", "10", "write", "10", "cache", "10"),
);

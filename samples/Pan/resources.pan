unique template site/one/resources;

# datastores templates
prefix "/software/components/opennebula/datastores/0";
"name" = "ceph.example";
"bridge_list" = list(FULL_HOSTNAME); # for now, do this from the headnode
"ceph_host" = CEPH_MON_HOSTS;
"ceph_secret" = CEPH_LIBVIRT_UUID;
"ceph_user" = "libvirt";
"ceph_user_key" = CEPH_LIBVIRT_SECRET;
"datastore_capacity_check" = true;
"pool_name" = "one";
"type" = "IMAGE_DS";
"rbd_format" = 2;

prefix "/software/components/opennebula/datastores/1";
"name" = "nfs.example";
"datastore_capacity_check" = true;
"ds_mad" = "fs";
"tm_mad" = "shared";
"type" = "IMAGE_DS";

# untouchables resources
prefix "/software/components/opennebula/untouchables";
"datastores" = list('system');

# extra conf
prefix "/software/components/opennebula";
"ssh_multiplex" = true;
"tm_system_ds" = "ssh";

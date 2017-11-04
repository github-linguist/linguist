structure template site/nagios/hosts/cluster-A;

# let Nagios server A monitor B
# just an example to make the templates compile
"nagios-slave-B.example.org" = create (NAGIOS_QUATTOR_HOST);
"nagios-slave-B.example.org/alias" = "slave B";
"nagios-slave-B.example.org/hostgroups" = list( "quattor-nodes" );


# "another-host-in-A.example.org" = create (NAGIOS_QUATTOR_HOST);
# "another-host-in-A.example.org/alias" = "another monitored host in cluster A";

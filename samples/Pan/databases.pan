template site/databases;

# Defines the mapping between the full hostname and the IP
# address.
final variable DB_IP = dict(
    escape("one"), "192.168.0.24",
    escape("hyp01"), "192.168.0.25",
    escape("vm"), "192.168.0.26",
);

# Defines the mapping between the full hostname and the
# physical machine.
# A different hardware template must be used for each machine
final variable DB_MACHINE = dict(
    escape("one"), "hardware/machine/ibm/x3550/x_KDXXXX",
    escape("hyp01"), "hardware/machine/ibm/hs21xm/blade_99HXXXX",
    escape("vm"), "hardware/machine/one/example",
);

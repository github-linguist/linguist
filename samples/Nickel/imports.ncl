# test = 'ignore'

# Nickel can import plain yaml, or json files
let
  data_users = (import "data_users.yml"),
  data_groups = (import "data_groups.json"),
  # or even toml
  data_machines = (import "data_machines.toml"),
  # And of course other nickel files
  data_nickel_properties = (import "data_nickel_properties.ncl"),
in

# This is the output object
{
  users = data_users.users,
  groups = data_groups.groups,
  machines = data_machines.machines,
  off_topic = {
    nickel_properties = data_nickel_properties,
  }
}

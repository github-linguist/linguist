define example::expiringhost($ip, $timestamp) {

  # Calculate the age of this resource by comparing 'now' against $timestamp
  $age = inline_template("<%= require 'time'; Time.now - Time.parse(timestamp) %>")

  # Max age, in seconds.
  $maxage = 60

  if $age > $maxage {
    $expired = true
    notice("Expiring resource $class[$name] due to age > $maxage (actual: $age)")
  } else {
    $expired = false
    notice("Found recently-active $class[$name] (age: $age)")
  }

  # I set target to a /tmp path so you can run this example as non-root.
  # In production, you probabyl won't set target as it defaults to /etc/hosts
  # (or wherever puppet thinks your platform wants it)
  host {
    $name:
      ip => $ip,
      target => "/tmp/expiring-hosts-example-output",
      ensure => $expired ? { true => absent, false => present };
  }
}

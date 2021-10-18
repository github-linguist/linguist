package sshd.authz

import input.pull_responses
import input.sysinfo

import data.hosts

# By default, users are not authorized.
default allow = false

# Allow access to any user that has the "admin" role.
allow {
    data.roles["admin"][_] = input.sysinfo.pam_username
}

# Allow access to any user who contributed to the code running on the host.
#
# This rule gets the `host_id` value from the file `/etc/host_identity.json`.
# It is available in the input under `pull_responses` because we
# asked for it in our pull policy above.
#
# It then compares all the contributors for that host against the username
# that is asking for authorization.
allow {
    hosts[pull_responses.files["/etc/host_identity.json"].host_id].contributors[_] = sysinfo.pam_username
}


# If the user is not authorized, then include an error message in the response.
errors["Request denied by administrative policy"] {
    not allow
}
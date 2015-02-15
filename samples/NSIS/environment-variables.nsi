ExpandEnvStrings $0 "%PATH%" ; Retrieve PATH and place it in builtin register 0.
ExpandEnvStrings $1 "%USERPROFILE%" ; Retrieve the user's profile location and place it in builtin register 1.
ExpandEnvStrings $2 "%USERNAME%" ; Retrieve the user's account name and place it in builtin register 2.

# activate this .talon file if the current app name is "Chrome"
# you can find app names by running ui.apps() in the REPL
app.name: Chrome
-
# key_wait increases the delay when pressing keys (milliseconds)
# this is useful if an app seems to jumble or drop keys
settings():
    key_wait = 4.0

# activate the global tag "browser"
tag(): browser

# define some voice commands
hello chrome: "hello world"
switch tab: key(ctrl-tab)
go to google:
    # note: use key(cmd-t) on Mac
    key(ctrl-t)    insert("google.com")
    key(enter)
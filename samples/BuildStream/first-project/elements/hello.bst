kind: import

# Use a local source to stage our file
sources:
- kind: local
  path: hello.world

# Configure the import element
config:

  # Place the content staged by sources at the
  # root of the output artifact
  target: /

#!/usr/bin/env python3

# Read the languages.yml file
with open('/home/engine/project/lib/linguist/languages.yml', 'r') as f:
    lines = f.readlines()

# Find the line after CUE and before CWeb
insert_index = None
for i, line in enumerate(lines):
    if line.strip() == 'CWeb:':
        insert_index = i
        break

if insert_index is None:
    print("Could not find CWeb entry")
    exit(1)

# Prepare the COW entry
cow_entry = '''COW:
  type: programming
  extensions:
  - ".cow"
  tm_scope: none
  ace_mode: text
  color: "#9E9E9E"
'''

# Insert the COW entry before CWeb
lines.insert(insert_index, cow_entry)

# Write the modified content back
with open('/home/engine/project/lib/linguist/languages.yml', 'w') as f:
    f.writelines(lines)

print("Successfully added COW to languages.yml")

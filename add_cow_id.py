#!/usr/bin/env python3

# Read the languages.yml file
with open('/home/engine/project/lib/linguist/languages.yml', 'r') as f:
    content = f.read()

# Replace the COW entry to add the language_id
old_entry = '''COW:
  type: programming
  extensions:
  - ".cow"
  tm_scope: none
  ace_mode: text
  color: "#9E9E9E"
'''

new_entry = '''COW:
  type: programming
  extensions:
  - ".cow"
  tm_scope: none
  ace_mode: text
  color: "#9E9E9E"
  language_id: 422342495
'''

content = content.replace(old_entry, new_entry)

# Write the modified content back
with open('/home/engine/project/lib/linguist/languages.yml', 'w') as f:
    f.write(content)

print("Successfully added language_id to COW entry")

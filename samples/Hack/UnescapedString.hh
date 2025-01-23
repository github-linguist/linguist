<?hh // strict

// Outside of this file, no one knows that UNESCAPED_STRING is a string
newtype UNESCAPED_STRING = string;

// This is how we initially taint a string.
function unescaped_string(string $s): UNESCAPED_STRING {
  return $s;
}

// This is the only thing you can do with an UNESCAPED_STRING (other than pass
// it around)
function escape_unescaped_string(UNESCAPED_STRING $s): string {
  // Your use case will decide how you want to escape your strings
  return sprintf('Escaped ---> "%s" <--- Escaped', $s);
}

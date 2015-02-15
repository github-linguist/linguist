$string = 'I am a string';
# Test
if (preg_match('/string$/', $string))
{
    echo "Ends with 'string'\n";
}
# Replace
$string = preg_replace('/\ba\b/', 'another', $string);
echo "Found 'a' and replace it with 'another', resulting in this string: $string\n";

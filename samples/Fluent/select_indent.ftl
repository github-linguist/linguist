select-1tbs-inline = { $selector ->
   *[key] Value
}

select-1tbs-newline = {
$selector ->
   *[key] Value
}

select-1tbs-indent = {
    $selector ->
   *[key] Value
}

select-allman-inline =
{ $selector ->
   *[key] Value
    [other] Other
}

select-allman-newline =
{
$selector ->
   *[key] Value
}

select-allman-indent =
{
    $selector ->
   *[key] Value
}

select-gnu-inline =
   { $selector ->
      *[key] Value
   }

select-gnu-newline =
   {
$selector ->
      *[key] Value
   }

select-gnu-indent =
   {
       $selector ->
      *[key] Value
   }

select-no-indent =
{
$selector ->
*[key] Value
[other] Other
}

select-no-indent-multiline =
{
$selector ->
*[key] Value
       Continued
[other]
    Other
    Multiline
}

# # ERROR (Multiline text must be indented)
# select-no-indent-multiline = { $selector ->
#    *[key] Value
# Continued without indent.
# }

select-flat =
{
$selector
->
*[
key
] Value
[
other
] Other
}

# Each line ends with 5 spaces.
select-flat-with-trailing-spaces =
{
$selector
->
*[
key
] Value
[
other
] Other
}

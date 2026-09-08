module network::RouterConfig

/*
:local timeout 10
/system identity
*/
str configureBridge(str name) =
  "
/interface bridge
add name=<name>
:put \"Bridge configured\"
";

module network::RouterConfig

/*
:local timeout 10
/system identity
*/
str configureBridge(str name) =
  "
:local bridgeName \"<name>\"
/interface bridge
add name=$bridgeName
:put \"Bridge configured\"
";

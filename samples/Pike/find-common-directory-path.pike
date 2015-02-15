array paths = ({ "/home/user1/tmp/coverage/test",
                 "/home/user1/tmp/covert/operator",
                 "/home/user1/tmp/coven/members" });

// append a / to each entry, so that a path like "/home/user1/tmp" will be recognized as a prefix
// without it the prefix would end up being "/home/user1/"
paths = paths[*]+"/";

string cp = String.common_prefix(paths);
cp = cp[..sizeof(cp)-search(reverse(cp), "/")-2];
Result: "/home/user1/tmp"

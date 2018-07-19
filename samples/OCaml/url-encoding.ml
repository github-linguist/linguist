$ ocaml
# #use "topfind";;
# #require "netstring";;

# Netencoding.Url.encode "http://foo bar/" ;;
- : string = "http%3A%2F%2Ffoo+bar%2F"

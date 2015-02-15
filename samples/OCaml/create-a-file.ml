# let oc = open_out "output.txt" in
  close_out oc;;
- : unit = ()

# Unix.mkdir "docs" 0o750 ;; (* rights 0o750 for rwxr-x--- *)
- : unit = ()

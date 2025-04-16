package kubernetes.admission                                                # line 1

deny[msg] {                                                                 # line 2
  input.request.kind.kind == "Pod"                                          # line 3
  image := input.request.object.spec.containers[_].image                    # line 4
  not startswith(image, "hooli.com/")                                       # line 5
  msg := sprintf("image fails to come from trusted registry: %v", [image])  # line 6
}
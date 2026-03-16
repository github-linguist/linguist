# SPDX-License-Identifier: MIT
# hello.k9.ncl - Example K9 Self-Validating Component
#
# This is a minimal .k9 component demonstrating the pedigree structure.
# It validates itself against the pedigree schema.

let pedigree = import "../pedigree.ncl" in

pedigree.K9Pedigree & {
  metadata = {
    name = "hello-k9",
    version = "1.0.0-alpha",
    description = "A minimal K9 component that says hello",
  },

  target = {
    os = 'Linux,
    is_edge = false,
    requires_podman = false,
  },

  security = {
    trust_level = 'Yard,
    allow_network = false,
    allow_filesystem_write = false,
    allow_subprocess = false,
  },

  validation = {
    checksum = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    pedigree_version = "1.0.0",
    hunt_authorized = false,
  },

  recipes = {
    install = "echo 'Hello from K9!'",
    validate = "nickel typecheck examples/hello.k9.ncl",
    deploy = "echo 'K9 deployed successfully'",
    migrate = "echo 'No migration needed'",
  },
}

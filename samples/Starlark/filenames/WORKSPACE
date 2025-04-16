# rules for scala
# https://github.com/bazelbuild/rules_scala#getting-started
# pull rule definitions from git
git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala.git",
    commit = "73743b830ae98d13a946b25ad60cad5fee58e6d3", # update this as needed
)

# load the desired scala rules for this workspace
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

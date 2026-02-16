# Git Submodules

The Git submodule source uses `git submodule foreach` CLI commands to enumerate dependencies and properties.  The Git submodule dependency source requires `git` version 2.0 or greater to be available, that the project is a valid Git repository, and that submodules are fetched locally.

#### Nested submodules

Nested submodules are detected as long as the submodules have been fetched locally, i.e. `git submodule update --init --recursive` or a similar operation has been run.  If a nested submodule has not been initialized, it will not be detected and no error is provided.

Dependencies for submodules will be cached at a path similar to their nested structure to avoid any clashes if a single repository is references as a submodule in multiple places.  As an example if `my_project` uses `submodule_1`, and `submodule_2` uses `submodule_2`, the metadata files will be available at:

- <cache_path>
| - git_submodule
  | - my_project.txt
  | - my_project
    | - submodule_1.txt
    | - submodule_1
      | - submodule_2.txt

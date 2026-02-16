# Cargo

The cargo source will detect dependencies when `Cargo.toml` is found at an apps `source_path`.  The source uses the `cargo metadata` CLI and reports on all dependencies that are listed in the output in `resolve.nodes`, excluding packages that are listed in `workspace_members`.

## Metadata CLI options

Licensed by default runs `cargo metadata --format-version=1`.  You can specify additional CLI options by specifying them in your licensed configuration file under `cargo.metadata_options`.  The configuration can be set as a string, or as an array of strings for multiple options.

```yml
cargo:
  metadata_options: '--all-features'
```

```yml
cargo:
  metadata_options:
    - '--all-features'
    - '--filter-platform x86_64-pc-windows-msvc'
```

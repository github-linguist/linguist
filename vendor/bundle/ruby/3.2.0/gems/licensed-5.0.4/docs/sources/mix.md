# Mix

The mix source will detect `mix.lock` lockfiles, and parse their contents to enumerate dependencies.

The `mix` CLI tool itself isn't used to extract dependency data, and no project compilation occurs during license extraction, but your dependencies should be present in your project `deps/` directory, as is conventional with Elixir projects.

## Installing Dependencies

Your project should contain a `deps/` directory containing the dependency sources. You can download your dependencies (which are defined in `mix.exs`) using `mix`:

```
mix deps.get
```

This will create your `mix.lock` lockfile if needed.

Be sure to re-run this command whenever your `mix.exs` dependencies change to update your lockfile and dependency sources.

## Limitations

Because `mix deps.get` does not generate `mix.lock` entries for `:path` dependencies (nor are they stored in `deps/`), license information is not extracted from them.

If you need to extract license information from dependencies from another local directory (that is a Git repository), consider using a `:git` dependency with a file path in your `mix.exs`.
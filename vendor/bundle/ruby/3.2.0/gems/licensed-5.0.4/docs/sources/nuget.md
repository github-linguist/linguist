# NuGet

The NuGet source will detect ProjectReference-style restored packages by inspecting `project.assets.json` files for dependencies. It requires that `dotnet restore` has already ran on the project.

The source currently expects that `source_path` is set to the `obj` directory containing the `project.assets.json`.
For example, if your project lives at `foo/foo.proj`, you likely want to set `source_path` to `foo/obj`.
If in MSBuild you have customized your `obj` paths (e.g. to live outside your source tree), you may need to set `source_path` to something different such as `../obj/foo`.

### Search strategy
This source looks for licenses:
1. Specified by SPDX expression via `<license type="expression">` in a package's `.nuspec` (via licensee)
2. In license files such as `LICENSE.txt`, even if not specified in the `.nuspec` (via licensee)
3. Specified by filepath via `<license type="file">` in a package's `.nuspec`, even if not a standard license filename.
4. By downloading and inspecting the contents of `<licenseUrl>` in a package's `.nuspec`, if not found otherwise.
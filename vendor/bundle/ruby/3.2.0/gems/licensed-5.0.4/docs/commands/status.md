# `licensed status`

The status command finds all dependencies and checks whether each dependency has a valid record.  There are two methods for checking dependencies' statuses

## Checking status with metadata loaded from cached files

This is the default method for checking the status for dependencies and the recommended method for using licensed.  Checking status from cached metadata files will occur when the `--data-source` CLI flag is either unset, or set to `--data-source=files`.

When using `licensed status` in this scenario, licensed will only compile a minimal amount of dependency metadata like the dependency name and version to match against cached files.  Dependency license and notice texts are loaded from cached files that are created from the [licensed cache](./cache.md) command.

A dependency will fail the status checks if:

1. No cached record is found
2. The cached record's version is different than the current dependency's version
3. The cached record's `licenses` data is empty
4. The cached record's `license` metadata doesn't match an `allowed` license from the dependency's application configuration and the dependency has not been marked `reviewed` or `ignored`
   - If `license: other` is specified and all of the `licenses` entries match an `allowed` license a failure will not be logged
5. The cached record is flagged for re-review.
   - This occurs when the record's license text has changed since the record was reviewed.

## Checking status with computed metadata

Checking status with computed metadata and the licensed configuration will occur when the `--data-source` CLI flag is set to `--data-source=configuration`.

When using `licensed status` in this scenario, licensed will compile all dependency metadata and license text to use in status checks.  There is no need to run `licensed cache` prior to running `licensed status --data-source=configuration`.

A dependency will fail the status checks if:

1. The record's `licenses` data is empty
2. The record's `license` metadata doesn't match an `allowed` license from the dependency's application configuration and the dependency has not been marked `reviewed` or `ignored`
   - If `license: other` is specified and all of the `licenses` entries match an `allowed` license a failure will not be logged
   - A `reviewed` entry must reference a specific version of the depdency, e.g. `<name>@<version>`.  The version identifier must specify a specific dependency version, ranges are not allowed.

## Detect and alert on stale cached metadata files

Licensed can alert on any metadata files that don't correlate to a currently used dependency when `licensed status` is run.  To configure this behavior, set a root-level `stale_records_action` value in your [licensed configuration file](./../configuration.md).

Available values are:

1. `'error'`: Treat stale cached records as errors.  Licensed will output errors for any stale metadata files and will cause `licensed status` to fail.
1. `'warn'`, `''`, or unset (default): Treat stale cached records as warnings.  Licensed will output warnings for any stale metadata files but will not cause `licensed status` to fail.
1. `'ignore'`, any other value: Ignore stale cached records.  Licensed will not output any notifications about stale metadata files.

```yaml
# in the licensed configuration file
stale_records_action: 'warn'
```

## Options

- `--config`/`-c`: the path to the licensed configuration file
   - default value: `./.licensed.yml`
- `--sources`/`-s`: runtime filter on which dependency sources are run.  Sources must also be enabled in the licensed configuration file.
   - default value: not set, all configured sources
- `--format`/`-f`: the output format
   - default value: `yaml`
- `--data-source`/`-d`: where to find the data source of records for status checks
   - available values: `files`, `configuration`
   - default value: `files`
- `--force`: if set, forces all dependency metadata files to be recached
   - default value: not set

## Reported Data

The following data is reported for each dependency when the YAML or JSON report formats are used

- name: the licensed recognized name for the dependency including the app and source name
   - e.g. the full name for the `thor` bundler dependency used by this tool is `licensed.bundler.thor`
- allowed: true if the dependency has passed all checks, false otherwise
- version: the version of the enumerated dependency
- license: the dependency's SPDX license identifier
- filename: the full path on disk to the dependency's cached metadata file, if available
- errors: any error messages from failed status checks, if available

## Status errors and resolutions

### cached dependency record not found

*Cause:* A dependency was found while running `licensed status` that does not have a corresponding cached metadata file

*Resolution:* Run `licensed cache` to update the metadata cache and create the missing metadata file

### cached dependency record out of date

*Cause:* A dependency was found while running `licensed status` with a different version than is contained in the dependency's cached metadata file

*Resolution:* Run `licensed cache` to update the out-of-date metadata files

### missing license text

*Cause:* A license determination was made, e.g. from package metadata, but no license text was found.

*Resolution:* Manually verify whether the dependency includes a file containing license text.  If the dependency code that was downloaded locally does not contain the license text, please check the dependency source at the version listed in the dependency's cached metadata file to see if there is license text that can be used.

If the dependency does not include license text but does specify that it uses a specific license, please copy the standard license text from a [well known source](https://opensource.org/licenses).

### license text has changed and needs re-review. if the new text is ok, remove the `review_changed_license` flag from the cached record

*Cause:* A dependency that is set as [reviewed] in the licensed configuration file has substantially changed and should be re-reviewed.

*Resolution:* Review the changes to the license text and classification, along with other metadata contained in the cached file for the dependency.  If the dependency is still allowable for use in your project, remove the `review_changed_license` key from the cached record file.

### license needs review / dependency needs review

*Cause:* A dependency is using a license that is not in the configured [allowed list of licenses][allowed], and the dependency has not been marked [ignored] or [reviewed].
*Resolution:* Review the dependency's usage and specified license with someone familiar with OSS licensing and compliance rules to determine whether the dependency is allowable.  Some common resolutions:

1. The dependency's specified license text differed enough from the standard license text that it was not recognized and classified as `other`.  
   - This resolution only applies when checking dependency status [with cached metadata files](./#checking-status-with-metadata-loaded-from-cached-files).
   - If the cached license text is recognizable with human review then update the `license: other` value in the cached metadata file to the correct license. An updated classification will persist through version upgrades until the detected license contents have changed.  The determination is made by [licensee/licensee](https://github.com/licensee/licensee), the library which this tool uses to detect and classify license contents.
1. The dependency might need to be marked as [ignored] or [reviewed] if either of those scenarios are applicable.
   - When checking status [with computed metadata](./#checking-status-with-computed-metadata), a reviewed entry must include both the dependency's name and the version that it was reviewed at e.g. `licensed@3.8.0`
1. If the used license should be allowable without review (if your entity has a legal team, they may want to review this assessment), ensure the license SPDX is set as [allowed] in the licensed configuration file.

[allowed]: ../configuration/allowed_licenses.md
[ignored]: ../configuration/ignoring_dependencies.md
[reviewed]: ../configuration/reviewing_dependencies.md

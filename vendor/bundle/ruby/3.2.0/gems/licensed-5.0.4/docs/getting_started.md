# Getting Started

Licensed's core workflow is a multi-step process:

1. `licensed cache` ([docs](./commands/cache.md)) is run manually and/or in an automated workflow
   - Creates or updates files in a git repo containing metadata including licenses and other legal text for each dependency used by a project
1. `licensed status` ([docs](./commands/status.md)) is run manually and/or in an automated workflow
   - Validate that every detected dependency has a metadata file written in the repository, and that each dependency's stored metadata passes a number of compliance checks
1. Any detected errors/warnings are manually [resolved](./commands/status.md#status-errors-and-resolutions)
1. Repeat the above steps until all dependencies have metadata files stored in the repository and `licensed status` is not reporting any errors.

## Caching dependency metadata

Caching depedency metadata into the repository brings the metadata contents closer to the dependencies where they are used, making status validation faster and possible in offline scenarios.  Keeping metadata alongside your code in git gives teams an easily auditable trail for dependency updates over time, and ties into common review practices to ensure that changes aren't quietly ignored.

Caching metadata should be done whenever project code changes, to ensure that metadata files are in sync with the current state of the project code.

## Checking dependency metadata status

Dependency metadata checks verify that every dependency

- has a metadata file available, or has been explicitly ignored by the project owners or OSS experts
- is using an approved OSS license, or has been reviewed and signed off by an OSS expert
- is up to date with the current state of a project

Checking dependencies for compliance violations should be performed whenever code changes in a repository.  Moving compliance checks inline in the development workflow reduces friction later, and can even prevent costly situations later if a non-compliant dependency would need to be removed from a project.

## Automated workflows

Integrating github/licensed into your workflow can be tedious, and luckily there are a few automated tools available to make usage easier.

### Bundler

The [bundler-licensed plugin](https://github.com/sergey-alekseev/bundler-licensed) runs `licensed cache` automatically when using `bundler`.  See the linked repo for usage and details.

### GitHub Actions

The [licensed-ci](https://github.com/marketplace/actions/licensed-ci) GitHub Action runs `licensed` as part of an opinionated CI workflow and can be configured to run on any GitHub Action event to automatically update the cached metadata files and check their status.  See the linked action for usage and details.

The [setup-licensed](https://github.com/marketplace/actions/setup-github-licensed) GitHub Action installs `licensed` to the workflow environment.  See the linked actions for usage and details.

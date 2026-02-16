# CocoaPods

The cocoapods source will detect dependencies when `Podfile` and `Podfile.lock` are found at an app's `source_path`.  The cocoapods source uses the [cocoapods-dependencies-list](https://github.com/jonabc/cocoapods-dependencies-list) plugin to enumerate dependencies and gather metadata on each package.

**NOTE:  Licensed does not install the [cocoapods-dependencies-list](https://github.com/jonabc/cocoapods-dependencies-list) plugin.  Users must install the gem alongside the cocoapods gem to enumerate cocoapods dependencies.**

## Evaluating dependencies from a specific target

The `cocoapods.targets` property is used to specify which targets to analyze dependencies from. By default, dependencies from all targets will be analyzed.

```yml
cocoapods:
  targets:
    - ios
```

## Specifying which pod executable to run

The cocoapods source will call the `pod` executable to evaluate dependencies by default.  If needed, you can override the executable used with the `cocoapods.command` configuration option.  This might be useful if the full path to the `pod` executable is needed (e.g. `pod` is not findable from the system `PATH`), or if you need to execute `pod` with `bundle exec`.

```yml
cocoapods:
  command: 'bundle exec pod'
```

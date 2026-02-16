# Customize Licensee's behavior

Licensed uses [Licensee](https://github.com/licensee/licensee) to detect and evaluate OSS licenses for project dependencies found during source enumeration. Licensed can optionally [customize Licensee's behavior](https://github.com/licensee/licensee/blob/main/docs/customizing.md#customizing-licensees-behavior) based on options set in the configuration file.

**NOTE** Matching licenses based on package manager metadata and README references is always enabled and cannot currently be configured.

```yml
licensee:
  # the confidence threshold is an integer between 1 and 100. the value represents
  # the minimum percentage confidence that Licensee must have to report a matched license
  # https://github.com/licensee/licensee/blob/main/docs/customizing.md#adjusting-the-confidence-threshold
  confidence_threshold: 90 # default value: 98
```

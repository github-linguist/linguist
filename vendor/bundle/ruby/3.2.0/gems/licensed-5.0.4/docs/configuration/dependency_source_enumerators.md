# Specifying dependency sources to use in an application

**Key**: sources  
**Required**: false  
**Default value**: All sources enabled

The sources configuration property specifies which sources `licensed` will use to enumerate dependencies.
By default, `licensed` will try to enumerate dependencies from all sources.  As a result,
the configuration property should be used to explicitly disable sources rather than to enable a particular source.

This configuration value does not guarantee that a source will enumerate dependencies.  Each
configured source's `enabled?` method must return true for licensed to pull dependency information.

`licensed` determines which sources will try to enumerate dependencies based on the following rules:

1. If no sources are configured, all sources are enabled
2. If no sources are set to true, any unconfigured sources are enabled
3. If any sources are set to true, any unconfigured sources are disabled

```yml
# all other sources are enabled by default since there are no sources set to true
sources:
  bower: false

# all other sources are disabled by default because a source was set to true
sources:
  bower: true
```

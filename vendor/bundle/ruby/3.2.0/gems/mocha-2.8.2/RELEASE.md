# Release Notes

## 2.8.2

### External changes

* Improve source location of deprecation warning for `ParameterMatchers` matcher classes (c5171488)

## 2.8.1

### External changes

* Restore unqualified access to `ParameterMatchers` matcher classes, but deprecate such access (5231a4f7)
* Restore unqualified access to `ParameterMatchers::BaseMethods`, but deprecate such access (8a898567)

## 2.8.0

Many thanks to @etiennebarrie for his help in testing v3.0.0 release candidates which led to many of these changes.

### External changes

* Extract `ParameterMatchers::BaseMethods` module and deprecate inheriting from `ParameterMatchers::Base` class (0ddfbe4b)
* Move matcher builder methods into `ParameterMatchers::Methods` module and only include that module into `API` (2de41423)
* Provide deprecated access to matcher builder methods when including `ParameterMatchers` module (299488e1)
* Provide deprecated access to matcher classes, e.g. `ParameterMatchers::Equals`, from within tests/specs (dcced1b4)

### Internal changes

* Move `ParameterMatchers#parse_option` -> `HasEntry.parse_option` (9e2a6f66)

**WARNING: This release accidentally removed access to ParameterMatchers classes**
* Fully-qualified access from within a test/spec class still works, e.g. `Mocha::ParameterMatchers::HasKey`, but unqualified access does not work, e.g. `HasKey`.
* The mistake has been fixed in v2.8.1 where unqualified access is restored but deprecated. Unqualified access will be removed in v3.

## 2.7.1

### External changes

* Deprecate `Configuration#stubbing_method_on_nil=` (#694)
* Indicate when parameter matcher logic is defined by block passed to `Expectation#with` (#698, b30e4434)
* Improve documentation for `Expectation#with`, especially when it is passed a block (#698, #682, #606 & #681)

## 2.7.0

### External changes

* Fail fast if invocation matches never expectation (#679, #678, #490, #131 & #44) - thanks to @ducmtran & @davidstosik for reporting

### Internal changes

* Workaround for JRuby jar-dependencies issue (#690)
* Ruby v3.4 stacktrace uses single-quote vs backtick (#688 & #689) - thanks to Vít Ondruch

**WARNING: This release fixes a very _old_ bug**
* The bug relates to the use of `Expectation#never` in combination with other expectations on the same method.
* Please ensure you fix the relevant deprecation warnings when running against v2.6.1 *before* upgrading to v2.7.0.
* Previously, the following test would have passed, but now it will fail with an unexpected invocation error on the `foo.bar` line.

    foo = mock('foo')
    foo.stubs(:bar)
    foo.expects(:bar).never
    foo.bar

## 2.6.1

### External changes

* Fix logic for displaying deprecation warning for expectation with never cardinality (#686) - thanks to @davidstosik for reporting

## 2.6.0

### External changes

* Expectation with never cardinality should display deprecation warning (#681) - thanks to @ducmtran for reporting and testing

**WARNING: This release results in some incorrect deprecation warnings:**
* The logic for displaying the deprecation warnings is fixed in v2.6.1 (#686).

### Internal changes

* Simplify backtrace related assertions (#680)
* Remove unused `ExpectationList#match_but_out_of_order` (f2fa9919)

## 2.5.0

### External changes

* Add metadata to gem specification, including `changelog_uri` (#608, eb1b8ea2) - thanks to @mattbrictson
* Fix warnings in Ruby v3.4 (#672, #669) - thanks to @radville for reporting
* Add warnings & notes about regressions, known issues, etc to release notes (#675, #676 & #677) - thanks to @davidstosik

### Internal changes

* Fix `jaro_winkler` compilation errors on MacOS (5c7d14cb)
* Fix typos in `IncludesTest` test names (6fb5a5a6)
* Fix `rubocop` version constraint for Ruby > v2.2.0 (d5c6b98a)

## 2.4.5

### External changes

* Fix regression when stubbed method expects `Hash` but receives `ActionController::Parameters` object (#662, #664) - thanks to @evgeni for reporting and testing

## 2.4.4

### External changes

* Fix regression when method expecting `Hash` parameter or keyword arguments is invoked with no arguments (#662, #663) - thanks to @vlad-pisanov for reporting

**WARNING: This release includes a regression:**
* A `NoMethodError` was raised when a stubbed method was expecting a `Hash`, but was invoked with an instance of `ActionController::Parameters`. See #662 for the report and #664 for the fix which was released in v2.4.5.

## 2.4.3

### External changes

* Fix regression when matching `Hash` parameter or keyword arguments (#657, #660) - thanks to @josesei for reporting and testing

**WARNING: This release inadvertently introduced a couple of regressions:**
* A `NoMethodError` was raised when a stubbed method was expecting a `Hash`, but was invoked with no arguments, e.g. with `C.expects(:foo).with(bar: 42)` and invoking `C.expects(:foo)`. See #662 for the report and #663 for the fix which was released in v2.4.4.
* A `NoMethodError` was raised when a stubbed method was expecting a `Hash`, but was invoked with an instance of `ActionController::Parameters`. See #662 for the report and #664 for the fix which was released in v2.4.5.

## 2.4.2

### External changes

* Don't trust `Object#is_a?` in presence of mock objects (#656) - thanks to @casperisfine

**WARNING: This release includes a regression:**
* Keyword argument and top-level `Hash` matching became more relaxed than intended, e.g. `mock.expects(:method).with(key: "value")` accepted `mock.method(key: "value", key2: "value")` when it should not have done. See #657 & #675 for the reports and #660 for the fix which was released in v2.4.3.

## 2.4.1

### External changes

* Fix regression in matchers when used with keyword arguments (#654 & #655) - thanks to @ElvinEfendi for reporting

### Internal changes

* Reduce duplication & consolidate `#to_matcher` method definitions (600ee2aa, e9de64e4, #655)
* Change `#to_matcher` method to use keyword arguments (3b60b7df, #655)

**WARNING: This release includes a regression:**
* Keyword argument and top-level `Hash` matching became more relaxed than intended, e.g. `mock.expects(:method).with(key: "value")` accepted `mock.method(key: "value", key2: "value")` when it should not have done. See #657 & #675 for the reports and #660 for the fix which was released in v2.4.3.

## 2.4.0

### External changes

* Improve rendering of keyword arguments (#652) - thanks to @casperisfine

### Internal changes

**WARNING: This release includes a couple of regressions:**
* Nested parameter matching for keyword arguments became more relaxed than intended, e.g. `mock.expects(:method).with(has_entry(:k1, k2: 'v2'))` accepted `mock.method(k1: { k2: 'v2', k3: 'v3' })` when it should not have done. See #654 for the report and #655 for the fix which was released in v2.4.1.
* Keyword argument and top-level `Hash` matching became more relaxed than intended, e.g. `mock.expects(:method).with(key: "value")` accepted `mock.method(key: "value", key2: "value")` when it should not have done. See #657 & #675 for the reports and #660 for the fix which was released in v2.4.3.

* Improvements to `#mocha_inspect` unit tests (#650)

## 2.3.0

### External changes

* Fix nested parameter matching for keyword arguments (f94e2504, #648) - thanks to @CodingAnarchy for reporting

**WARNING: This release inadvertently introduced a couple of regressions:**
* Nested parameter matching for keyword arguments became more relaxed than intended, e.g. `mock.expects(:method).with(has_entry(:k1, k2: 'v2'))` accepted `mock.method(k1: { k2: 'v2', k3: 'v3' })` when it should not have done. See #654 for the report and #655 for the fix which was released in v2.4.1.
* Keyword argument and top-level `Hash` matching became more relaxed than intended, e.g. `mock.expects(:method).with(key: "value")` accepted `mock.method(key: "value", key2: "value")` when it should not have done. See #657 & #675 for the reports and #660 for the fix which was released in v2.4.3.

## 2.2.0

### External changes

* Support multiple methods in `responds_with` matcher (f086b7e4, #578) - thanks to @vlad-pisanov for the suggestion
* Add block syntax for sequences (93fdffd, #61)
* Improve sequence failure message (0800c6ff, #60)
* Drop support for Ruby v2.0 (85848fb0, #642)
* Include the original test name in expired stub error messages (ca3ff8eb, #641, #642) - thanks to @casperisfine

* Avoid rubocop directive ending up in YARD docs (2a9ee81a)
* Update docs to fix those for `Mock#method_missing` (cee0bad6)
* Reinstate missing CNAME for GitHub Pages site (da67bb0d)
* Use Ruby v1.9 Hash syntax in docs (6de20726, #625)
* Add missing YARD tag for API#sequence name param (343c5979)
* Add missing YARD tag for API#states name param (f798df83)

### Internal changes

* Tidy up Minitest vs MiniTest references (#626, #614, #615) - thanks to @zenspider & @Maimer for their help
* Add Ruby v3.3 to CI build matrix (ce31b544)

## 2.1.0

### External changes

* Fix compatibility with Minitest (#614) - thanks to @kyrofa & @manewitz for reporting and to @zenspider for his input

### Internal changes

* Update URLs for links to Ruby & MIT licenses (d6470af4)

## 2.0.4

### Internal changes

* Update `README.md` (e8c21e1b)

## 2.0.3

### External changes

* Fix `BacktraceFilter` to handle special characters (e242033f, #592) - thanks to @casperisfine

### Internal changes

* Add Ruby v3.1 to the CircleCI build (3e460489)
* DRY up `regexp_matches` test (ae9fed4a)
* Fix regexp_matches tests in Ruby v3.2 (26b106a5, #590)
* Use Ruby 1.9 hash syntax (8bc0ad2f, #598, #537) - thanks to @herwinw
* Simplify storage of `MOCHA_OPTIONS` (b70507a1, #600) - thanks to @herwinw
* Pin JRuby to v9.3.9.0 in CircleCI builds (b8e6d064, #591)
* Rubocop: enable Style/FormatStringToken cop (089a688e, #603) - thanks to @herwinw
* Remove Ruby version check from `RespondsLikeTest` (21583129)
* Add Ruby v3.2 to CircleCI build (f7e17636, #601)
* Use Ruby v2.6 vs v2.2 to run lint CI job (af40b7db)
* Pin yard version to v0.9.28 to avoid `ArgumentError` (12f1eef7)
* Revert "Pin JRuby to v9.3.9.0 in CircleCI builds" (4f5bb2f0, #591)
* Remove invalid CircleCI token from badge URL (7078e76a)
* Revert "Pin yard version to v0.9.28 to avoid ArgumentError" (7c6c10c5, #609)
* Remove Google Analytics tracking code (2279c49d, #612)
* Update `MIT-LICENSE.md` (48162b4e)
* Update `COPYING.md` (f3152376)

## 2.0.2

### External changes

* Fix regression in `Mock#responds_like` behaviour - thanks to @adrianna-chang-shopify for reporting (#580,#583,ba4d619e)

## 2.0.1

### External changes

* Fix `LoadError` when using v2.0.0 with Ruby < v2.7 by moving declaration of runtime dependency on `ruby2_keywords` gem from `Gemfile` to `mocha.gemspec` - thanks to @mishina2228 for reporting (#581, #582, cdeb0356)

## 2.0.0

### External changes

* Remove support for Ruby v1.9 - thanks to @wasabigeek (#552)
* Support strict keyword argument matching - see docs for `Expectation#with` & `Configuration#strict_keyword_argument_matching=` - thanks to @wasabigeek (#446,#535,#544,#562)
* Deprecate `Hash` args that don't strictly match (#563,981c31be)
* Drop support for older versions of test-unit - gem versions of test-unit earlier than v2.5.1 and versions of test-unit
from the Ruby v1.8 standard library are no longer supported (#540,969f4845)
* Drop support for older versions of minitest - versions of minitest earlier than v3.3.0 are no longer supported (#541,ca69dc9e)
* Remove deprecated `mocha/setup.rb` mechanism (642a0ff4)
* Add missing docs for `API#stub` parameter (257b4cb4)
* Remove optional reinstatement of v1.9 behaviour (#436,#438,#569,1473ee25)
* Remove deprecated methods in `Configuration` (#421,e7ff7528)
* Fail fast when mock receives invocations in another test (#440,#442,cb054d59)
* Improve docs re using matchers in `Expectation#with` (da7237cd)
* Expand `Expectation#with` docs re keyword arguments (fed6808d)
* Improve docs for `strict_keyword_argument_matching` (8d8f881d)
* Remove deprecated Rails plugin `init.rb` file (1c617175)
* Improve strict keyword argument matching deprecation warning by including the source location of the stub definition (77c0d4cc)
* Add README section re semantic versioning (00758246)

### Internal changes

* Separate linting from tests in terms of Rake tasks & CircleCI jobs - thanks to @wasabigeek (#556)
* Remove tests specific to Ruby v1.8 behaviour (46fca7ac, 3b369e99)
* Multi-line rubocop disable in `Mock#method_missing` (af2194c4)
* Remove unused arg for `HashMethods#mocha_inspect` (4f59e27f)
* Improve test runner assertions - failure vs error (eec7200a)
* Improve test coverage of `PositionalOrKeywordHash` (c294fe70)
* More consistent Test::Unit & Minitest integration (27dd3817)
* Remove redundant `require` statements (d82218a8,fa17b114)
* Add missing `require` statement (73493761)
* Disable Style/Semicolon cop globally (8cd0b705)

## 1.16.1

### External changes

* Fix regression in `Mock#responds_like` behaviour - thanks to @adrianna-chang-shopify for reporting (#580,#583,77af2af1)

## 1.16.0

### External changes

* Default `Configuration#reinstate_undocumented_behaviour_from_v1_9=` to `false` (6fcaf947)
* Deprecate `Configuration#reinstate_undocumented_behaviour_from_v1_9=` (a797c5fd)

### Internal changes

* Remove redundant deprecation disabling in MockTest (dc8ca969)

## 1.15.1

### External changes

* Fix regression in `Mock#responds_like` behaviour - thanks to @adrianna-chang-shopify for reporting (#580,#583,c586a08c)

## 1.15.0

### External changes

* Fix examples using mock constructor with block (1cc17667)
* Add another example for `API#sequence` (b7a7d233, #59)
* Remove support for Ruby v1.8 (ddb5d672)
* Deprecate support for Ruby versions earlier than v2.0 - thanks to @wasabigeek (#553, #555)

### Internal changes

* Update instructions for obtaining Rubygems API key (ed9c040a)
* Consistent definitions for `respond_to?` methods (#533)
* Run test tasks before release tasks (92a1bc6e, #447)
* Fix test:performance Rake task (#538, #539)
* Tidying following removal of support for Ruby v1.8 - thanks to @nitishr (#542)
* Remove `ParametersMatcher` from `Invocation#call_description` - thanks to @wasabigeek (#543)
* Remove unnecessary splatting in Invocation - thanks to @wasabigeek (#549)
* Extract `handle_method_call` from `method_missing` - thanks to @wasabigeek (#550)

## 1.14.0

### External changes

* Mock#expects,#stubs should return last expectation - thanks to @vlad-pisanov for #524 (b6b637db)

### Internal changes

* Avoid breaking change in psych v4 in ruby v3.1 (08b9f4ca)
* Remove broken Dependabot badge from README (d446657a)
* Add Ruby 3.0 to the CI matrix - thanks to @mishina2228 for #526 (65bc626e)
* Move development dependencies from gemspec to Gemfile - thanks to @mishina2228 for #527 (dd127f7b)

## 1.13.0

### External changes

* Add `ParameterMatchers#has_keys` - thanks to @cstyles for #512 (18d8104)
* Fix misleading exception message in `ParameterMatchers#has_entry` - thanks to @cstyles for #513 (9c4ef13)
* Only add dependency on rubocop if we're actually going to use it (f2f879f)
* Fix rake dependency constraints for older Ruby versions (7ce5f29)

### Internal changes

* Check documentation can be generated as part of CircleCI build (b30d9a9)
* Add --fail-on-warning option to yard rake task (53a6ee3)
* Add a weekly scheduled build to the CircleCI build (fd2a4c6)
* Add Ruby v1.8 to the CircleCI build matrix (818ca03)
* Add API token to fix CircleCI badge in README (607c5aa)
* Provide wrapped option for #mocha_inspect on hashes & arrays (d8f44bc)
* Use CircleCI instead of TravisCI for automated builds (c98c6ec)
* Switch to newer default Travis CI build env (c78f75c)
* Use latest Ruby versions in Travis CI builds (9e0043a)
* Use latest JRuby v9.2.18 in Travis CI builds (8c99a1b)
* Use consistent JRuby versions in Travis CI builds (0f849aa)
* Use more recent version of JRuby in Travis CI build matrix (58653db)

## 1.12.0

### External changes

* Various improvements to README inspired by #207 and #390 - thanks to @nitishr for his work on #390 (fed0eee6)
* Improve documentation related to `StateMachine` classes - thanks to @nitishr (#425 & #427)
* Fix regression in cardinality introduced in v1.10.0 (59454a8) and reported in #473 - thanks to @srvance for reporting and @nitishr for fixing (#474)
* Fix documentation for `Mocha::Expectation#when` - thanks to @olleolleolle (b4f59daa & #477)
* Remove `Mocha::Mock#respond_to?` from documentation - thanks to @nitishr (#480)
* Improvements to documentation for `Expectation#yields` & `#multiple_yields` - thanks to @andyw8 for reporting in #495 (1b6571c)
* Remove documentation & tests from gem to reduce its size by over 50% - thanks to @gabetax (#500)
* Update documentation to point to travis-ci.com instead of travis-ci.org

### Internal changes

* Refactor `StateMachine`-related classes - thanks to @nitishr (#425 & #427)
* Remove redundant test - thanks to @nitishr (8e4f1a7c)
* Add Ruby 2.7 to Travis CI matrix - thanks to @bastelfreak (fc5ea2f2)
* Simplify `Mockery` - thanks to @nitishr (#449)
* Update Travis CI badge to point to main vs master branch (bd8028f8)
* Generate docs using newer version of yard (v0.9.25) (c619afac)
* Manually upgrade jquery in docs from v1.7.1 -> v1.9.0 to fix CVE-2017-16011 (211098a5, dd5eeedb & 1b76e4d5; also see #492)
* Remove reference to non-existent jquery source map to fix error in Chrome developer tools (20156555)
* Temporarily ignore Ruby v1.8.7 build failures (e5b9feef)

## 1.11.2

### External changes

* Fix regression introduced in v1.10.0 that meant `Object#inspect` was called unnecessarily (368abd98)
* Warn when mock object receives invocations in another test - thanks to @nitishr (#442)
* Avoid rubocop comments appearing in YARD-generated docs (d8019eed)

### Internal changes

* Replace `StubbedMethod#original_method` & `#original_visibility` attribute reader methods with instance variables - thanks to @nitishr (d917f332)
* Set up `MochaExampleTest` & `StubbaExampleTest` as acceptance tests - thanks to @nitishr (4881cc58)
* Delete unused `PrettyParameters` class - thanks to @nitishr (314ea922)

## 1.11.1

### External changes

* The `reinstate_undocumented_behaviour_from_v1_9` configuration option is now enabled by default to give people a chance to see and fix the relevant deprecation warnings before the behaviour is removed in a future release (b91b1c9e)

## 1.11.0

### External changes

* Add `Expectation#with_block_given` & `Expectation#with_no_block_given` (#441).
  * Allows non-deprecated solution for #382. Thanks to @yemartin for reporting and to @techbelly & @nitishr for feedback.
* Fix issue with non-Array arguments passed to `Expectation#multiple_yields` (#444).
  * The undocumented behaviour is now properly supported and documented.

### Internal changes

* Move static YARD options from Rake task to `.yardopts` file - thanks to @nitishr (#429)
* Simplify implementation of yielding functionality - thanks to @nitishr (#439)
* Add missing require statement to `acceptance_test_helper.rb` (1070fc02)
* Add some baseline acceptance tests for yielding behaviour (c2cac911)
* Display a sponsor button on GitHub repo page (9fc5911b)
* Use new Deprecation.warning behaviour in `Invocation#call` (932d1166)

## 1.10.2

* Optionally reinstate undocumented behaviour from v1.9. This introduces a new configuration option (`reinstate_undocumented_behaviour_from_v1_9`) to reinstate a couple of bits of undocumented behaviour from v1.9 which were changed in v1.10 without any prior deprecation warning (#438):
  * The behaviour of `API#mock`, `API#stub` and `API#stub_everything` when called with a symbol as the first argument.
  * The behaviour of `Expectation#yields` and `Expectation#multiple_yields` when the stubbed method is called without a block.

## 1.10.1

* Ensure ObjectMethods & ClassMethods included when API extended (43778756)
* Fix regression in `any_instance` stubbing of methods on object which has an implementation of `#respond_to?` that depends on the object's internal state - thanks to @rafaelfranca for reporting & @nitishr for fixing (#432, #434, 469d4b17)

## 1.10.0

* Improve deprecation warning when requiring 'mocha/setup' (388f44d7)
* Add documentation for Cucumber integration (13ab797b)
* Add documentation about an undocumented feature of `API#mock`, `API#stub` & `API#stub_everything` being changed (7ed2e4e7, d30c1717)

**WARNING: This release inadvertently changed some undocumented behaviour:**
* An undocumented feature of `API#mock`, `API#stub` & `API#stub_everything` was changed. Previously when these methods were passed a single symbol, they returned a mock object that responded to the method identified by the symbol. Now Passing a single symbol is equivalent to passing a single string, i.e. it now defines the 'name' of the mock object.

## 1.10.0.beta.1

* Hide `ClassMethods#method_visibility` & `#method_exists?` methods to avoid clash with Rails (#428)

## 1.10.0.alpha

### External changes

* Remove dependency on metaclass gem (#49, #365)
* Accept symbol (as well as a string) as mock/stub name - thanks to @nitishr (#347, #353, #377)
* More realistic examples in documentation for `Expectation#yields` and `#multiple_yields` - thanks to @nitishr (#352, #383)
* Improve documentation for `Mock#responds_like` & `#responds_like_instance_of` - thanks to @nitishr (#337, #384)
* Make `Expectation#yields` & `Expectation#multiple_yields` fail when the caller of the stubbed method does not provide a block. This is a change to an undocumented aspect of the public API's behaviour. If this causes your tests to fail, then fix it by removing the unnecessary call to `Expectation#yields` or `Expectation#multiple_yields` - thanks to @nitishr (#382)
* Document `MOCHA_OPTIONS` in README - thanks to @nitishr (#311, #386)
* Add documentation to explain how Mocha is intended to be used - thanks to @nitishr (#330, #385)
* Deprecation warning if integration using 'mocha/test_unit' or 'mocha/minitest' fails - thanks to @nitishr (#229, #389, c6032d0b)
* Require at least one specified sequence for `Expectation#in_sequence` - thanks to @nitishr (#79, #396, 9020248a)
* Make signatures of `Mock#unstub` & `ObjectMethods#unstub` consistent - thanks to @nitishr (#397, f04d437)
* Deprecate requiring 'mocha/setup' (36adf880)
* Optionally display matching invocations alongside expectations - thanks to @nitishr (#178, #394, 00f0540, #410)
* Put deprecations into effect (#400, #418):
  * Remove deprecated 'mocha_standalone.rb' & 'mocha/standalone.rb'
  * Fail fast if no test library loaded
  * Removed optional block for `Mocha::API#mock`, `#stub` & `#stub_everything`
  * Remove deprecated `ParameterMatchers#has_equivalent_query_string` method
  * Remove deprecated 'mocha/mini_test.rb'
* Fix typo in docs for `Mocha::Configuration.prevent` (266ce71c)
* New-style configuration (see documentation for `Mocha::Configuration`) (#407, #421)
* Deprecate support for Ruby versions earlier than v1.9 (#325, c5f8496d)
* Deprecate support for versions of test-unit & minitest which need monkey-patching (a34e1a88)
* Deprecate old-style Rails plugin (#403, 2df77134)
* Documentation fixes & improvements which also fix YARD warnings (472d5416, a2c0d64a)

### Internal changes

* Pin minitest to v5.11.3 for Ruby v1.8.7 to fix build; minitest no longer supports Ruby v1.8.7 (4a0a580)
* Upgrade JRuby to v9.2.8.0 in Travis CI builds (aa29b3f)
* Only run rubocop for MRI Ruby versions & non-integration test builds (8f1c6af)
* Reduce duplication in any instance method class - thanks to @nitishr (#378)
* Simplify `AnyInstanceMethod`, `ClassMethod`, `InstanceMethod`, `ModuleMethod` class hierarchy - thanks to @nitishr (#381)
* Simplify `ClassMethods#method_exists?` & `ObjectMethods#method_exists?` making them consistent - thanks to @nitishr (#270, #362, #370)
* Don't override definition of `singleton_class` in `ClassMethods` - thanks to @nitishr (#391, #392)
* Do not include 'method_definer' methods into all objects (#268, #402)
* Distinguish different `ObjectMethods` modules (#268, #404)
* Pass invocation to expectation list methods - thanks to @nitishr (#408, #409, #411)
* Consistently use `assert_raises` - thanks to @nitishr (#405, #412, a66b7bed)
* Update Ruby & JRuby versions in Travis CI config (18cb1a93, eb061c53)
* Rubocop improvements (aa16ea67...6f4db70b, 2a1240e6...e95716ae)
* Fix inconsistency in CardinalityTest (aa10e0a8)
* Fix test failures on Mac OSX Catalina - thanks to @nitishr (#413, #417, #419, 8a0f2535)
* Remove default argument in `Expectation#invoke` - thanks to @nitishr (#414, #420)

## 1.9.0

* Add TruffleRuby to Travis CI build matrix - thanks to @deepj (#354)
* Explicitly set Travis CI OS to Ubuntu Trusty 14.04 (ded1fa45)
* Expand explanation of thread-safety concerns - thanks to @techbelly (#357)
* Refactor class method and any instance method - thanks to @chrisroos (#358)
* Rely on default bundler version in Travis CI builds (3352e9c5)
* Fix local build-matrix script (11abe231)
* No need to install latest bundler in build-matrix script (8247a894)

## 1.8.0

* Constrain rubocop version to avoid breaking Travis CI builds (05e507f5)
* Avoid calling Kernel#format from ObjectMethods#mocha_inspect - thanks to @hoffmanilya (#345)
* Fix build matrix script (#346)
* Avoid deprecation warning in gemspec (4976e0bc)
* Removed link to documentation translation (ef428ea2)
* Don't use the new bundler v2 in builds (683ded9b)
* Moved documentation from https://gofreerange.com/mocha/docs to https://mocha.jamesmead.org/ [683ded...a17fde](https://github.com/freerange/mocha/compare/683ded...a17fde)

## 1.7.0

* Update Ruby & JRuby versions in Travis CI config (9bf55631 & 3883af7e)
* Simplify gemspec (63744f86)
* Add rubocop and fix most cop violations (#341)
* Use Kernel#warn for deprecations - thanks to @etiennebarrie (#333, 196970a)

## 1.6.0

* Fix subtle bug in setting correct visibility of stubbed module methods on `Kernel` or `Object` - thanks to @chrisroos (#295)
* Avoid mocks for partial mocking leaking into subsequent tests - thanks to @skliew for reporting (#331)
* Remove OpenCollective badge, backers & sponsors (a283a079)
* Change gem version badge to SVG format and add SemVer stability badge - thanks to @greysteil (#335)
* Improve documentation for Configuration (#236)

## 1.5.0

* Prevent use of Mocha outside the context of a test/example - thanks to @andyw8 & @lzap (#327)

## 1.4.0

* Fix deprecation warning for `assert_nil` in `ClassMethodTest` (#308 & #309)
* Display file and line number in deprecation warning - thanks to @chrisarcand (#310, #312 & #313)
* Rename `mocha/mini_test.rb` to `mocha/minitest.rb` - thanks to @grosser (#320 & #322)
* Fix warning when delegating to mock in Ruby 2.4 - thanks to @tjvc (#321 & #323)
* Updates to Travis CI configuration ([73af600..9732726](https://github.com/freerange/mocha/compare/73af600...9732726) & 0426e5e)

## 1.3.0

* Ensure all tests run individually - thanks to @chrisroos (#267)
* Update Travis CI build status badge to show master branch status (#264)
* Correct RSpec section of the README - thanks to @myronmarston (0cc039c8)
* Fix pretty printing of quotes in `String#mocha_inspect` (#215 & #223)
* Add release instructions to README - thanks to @chrisroos (70a5febd & 3c664df7)
* Require at least Ruby v1.8.7 in gemspec - thanks to @knappe (3e20be8e)
* Remove redundant InstanceMethod#method_exists? - thanks to @chrisroos (8f58eddf)
* Reduce risk of hitting bug 12832 in Ruby v2.3 - thanks to @chrisroos (#277 & eca7560c)
* Fix JRuby build - thanks to @headius (jruby/jruby#4250) & @chrisroos (#274)
* Add latest stable version of JRuby to Travis CI build matrix (#288)
* Fix Ruby v1.8.7 builds on Travis CI (928b5a40 & 460dce5b)
* Deprecate passing block to mock object constructor (#290)
* Add a known issue to README for Ruby bug 12876 (#276)
* Add Ruby 2.4 and ruby-head to Travis CI build matrix - thanks to @junaruga (#297)
* Fix `Mocha::ParameterMatchers#includes` for `Array` values - thanks to @timcraft (#302)
* Use faster container-based virtual environments for Travis CI builds (#305)
* Rename `Mocha::ParameterMatchers::QueryStringMatches` to `QueryString` (#306)
* Handle blank parameter value for query string matcher - thanks to @weynsee (#303 & #304)
* Rename `Mocha::ParameterMatchers::QueryString` -> `EquivalentUri` (#307)
* Use `do ... end` instead of `{ ... }` in acceptance tests - thanks to @chrisroos (#294)

## 1.2.1

* Fixed #272. Workaround Ruby bug 12832 which caused interpreter to hang. See https://bugs.ruby-lang.org/issues/12832. Thanks to @chrisroos & @petems (6f1c8b9b, #273).

## 1.2.0

* Always use prepended module to stub class & instance methods for Ruby v2+ - thanks to @grosser & @chrisroos (43d56671, #244)
* Always use prepended module to stub AnyInstance methods in Ruby v2+ - thanks to @chrisroos (#262)
* Always set visibility of stub method to match stubbed method on included module - thanks to @grosser & @chrisroos (e87c03b0, #248)
* Always set visibility to stub method to match stubbed method on superclass - thanks to @chrisroos (38d902ad)
* Allow stubbing of method to which any instance responds (#200)
* Allow `includes` matcher to take matcher arguments - thanks to @lazyatom (#217)
* Avoid exception in older version of Rubygems - thanks to @chrisroos (78d930a7)
* Add licenses to gemspec as requested by @coreyhaines (#201)
* Fix typo in README - thanks to @jaredbeck (6119460d)
* Added section about using Mocha with RSpec & Rails to README (#221)
* Fix documentation for Mocha::API#stub method - thanks to @raeno (599b1dcd)
* Added backers and sponsors from OpenCollective - thanks to @piamancini (#253)
* Fix typo in docs for equals - thanks to @alexcoco (#254)
* Add known issue for Ruby v1.8 to README - thanks to @chrisroos (2c642096)

**WARNING: This release inadvertently introduced the possibility of causing the Ruby interpreter to hang:**
* There is a scenario where stubbing a class method originally defined in a module hangs the Ruby interpreter due to [a bug in Ruby v2.3.1](https://bugs.ruby-lang.org/issues/12832). See #272. This was fixed in Mocha v1.2.1.

## 1.1.0

* Set visibility of any instance stub method.
* Stub methods with a prepended method if there are other prepended methods. Thanks to @mrsimo.
* Improve docs for `Mock#responds_like` & `#responds_like_instance_of`.
* Use GitHub convention for instructions on contributing to Mocha.
* Fix typos in docs. Thanks to @10io

**WARNING: This release inadvertently introduced the possibility of causing the Ruby interpreter to hang:**
* From this release onwards, prepended modules have been used internally for stubbing methods. There is [an obscure Ruby bug](https://bugs.ruby-lang.org/issues/12876) in many (but not all) versions of Ruby between v2.0 & v2.3 which under certain circumstances may cause your Ruby interpreter to hang. See the Ruby bug report for more details. The bug has been fixed in Ruby v2.3.3 & v2.4.0.

## 1.0.0

### External changes
* Assume 'mocha' has been required when requiring 'mocha/setup'.
* Provide shortcuts for integrating with specific test library i.e. `require 'mocha/test_unit'` or `require 'mocha/mini_test'`
as alternatives to `require 'mocha/setup'`.
* Do not automatically try to integrate with test libraries. Since the automatic test library integration functionality
requires the test library to be loaded and this doesn't usually happen until *after* the bundle is loaded, it makes things
simpler if we use `require 'mocha/setup'` to explicitly setup Mocha when we know the test library has been loaded. Fixes #146 & #155.
* Consider stubs on superclasses if none exist on primary receiver. Largely based on changes suggested by @ccutrer in #145.
Note: this may break existing tests which rely on the old behaviour. Stubbing a superclass method and then invoking that
method on a child class would previously cause an unexpected invocation error. By searching up through the inheritance
hierarchy for each of the delegate mock objects, we can provide more intuitive behaviour. Instead of an unexpected invocation
error, invoking the method on the child class will cause the stubbed method on the superclass to be used.
* Avoid recursion when constructing unexpected invocation message. Fixes #168.
* Add explanation of method dispatch. Heavily based on the relevant jMock v1 documentation. Fixes #172.
* Make class_eval line number more accurate. This sets the line number as the line number of the `def` statement. Closes #169.
* Allow nesting of `responds_with` parameter matcher. Closes #166.
* Define `Mocha` module before it's referenced. The test helper defines a class `TestCase` within the `Mocha` module. When
running the tests inside the bundle, the `Mocha` module happens to be defined at this point. However when running the tests outside the bundle, it is not defined and so an exception is raised: `uninitialized constant Mocha (NameError)`. Fixes #163.
* Document lack of thread-safety. Fixes #154.
* Document how to use the build-matrix script. Fixes #160.
* Stubbing non-public method should use same visibility. This will probably break some existing tests that were somehow relying
on the stubbed method being public while the original method was protected or private. Fixes #150.

### Internal changes
* Use lastest Rubygems in Travis CI builds.
* Run the standard test suite against Ruby 2.1.0 in the build matrix.
* Run integration tests against Ruby 2.0.0 with latest Test::Unit gem in the build matrix.
* Test::Unit is not available in Ruby v1.9.3 standard library, so remove it from the build matrix.
* Force use of Test::Unit runner, etc in relevant integration tests. Prior to this, I don't think we were really testing the
Mocha integration with Test::Unit much, because, although `TestUnitTest` was a subclass of `Test::Unit::TestCase`, the
important test case instances are the temporary ones built by `TestRunner#run_as_test` et al. Prior to this change, these
would only have used Test::Unit where MiniTest was not available *at all* i.e. only in early versions of Ruby and when the
MiniTest gem was not loaded.
* Reset environment variables between build matrix builds.
* Only activate integration with relevant test library for each of the integration tests.
* Include standard build combinations from Travis CI config i.e. builds using standard library versions of test libraries.
* Fix `build-matrix.rb` script. Also use `.travis.yml` to decide what combinations to run. This means we
can now simulate the Travis CI build locally and avoid duplication. Fixes #157.
* Remove Ruby version map from build matrix script. I'm using the `rbenv-aliases` plugin to alias minor versions to the
relevant patch version.

## 0.14.0

* Official support for MiniTest v5. All tests now pass on the continuous integration build.

## 0.14.0.alpha

* Add speculative support for Minitest v5. Due to incompatibilities it has not yet been possible to run the Mocha test suite against Minitest v5. However, @zenspider (author of Minitest) provided the patch and he has tested it against Rails v4. Fixes #156. Thanks to @zenspider.
* Documentation updates.

## 0.13.3
* Allow `Mocha::ParameterMatchers#includes` to accept multiple items. Thanks to @simao.
* Allow stubbing of *private* `Kernel` methods. Fixes #134. Thanks to @camski for reporting.
* Avoid a warning when `test/unit/version` is required by other libraries in the same project. Fixes #140. Thanks to @tmiller.
* Make auto-activation of Test::Unit integration more resilient. This change is specifically to cope with the nasty re-defining of classes that is done by the `minitest-spec-rails` gem. Fixes #143. Thanks to @tubaxenor for reporting.
* Safer restoration of stubbed method visibility. Fixes #141. Thanks to @tmm1.
* Ensure `Mockery` instance gets reset even if exception raised. Fixes #144.
* Adapt Mocha acceptance tests to cope with changes in output from latest (v4.6.2) of MiniTest.
* Updates to README about Rails compatibility.

**NOTE: This release inadvertently caused deprecation warnings in some contexts:**
* When used with Rails v3.2.0-v3.2.12, v3.1.0-v3.1.10 & v3.0.0-v3.0.19.

## 0.13.2
* Stubbing of methods re-declared with different visibilty. Fixes #109.
* Add `Mock#responds_like_instance_of`. Fixes #119.
* Make `Expectation#inspect` less verbose and more useful. Fixes #122.
* Make unit tests more robust to changes in environment. Fixes #121.
* Update README in an attempt to head Rails-related issues off at the pass.
* Add a Gem Badge to provide a link to Mocha on Rubygems.
* Make documentation example consistent with other examples.

**NOTE: This release inadvertently caused deprecation warnings in some contexts:**
* When used with Rails v3.2.0-v3.2.12, v3.1.0-v3.1.10 & v3.0.0-v3.0.19.

## 0.13.1
* Fix #97 - `Mocha::ParameterMatchers#has_entry` does not work with an Array as the entry's value. Thanks to @ngokli.
* Allow deprecation `:debug` mode to be switched on from `MOCHA_OPTIONS` environment variable.

**NOTE: This release inadvertently caused deprecation warnings in some contexts:**
* When used with Rails v3.2.0-v3.2.12, v3.1.0-v3.1.10 & v3.0.0-v3.0.19.

## 0.13.0
* Major overhaul of MiniTest & Test::Unit integration. Mocha now integrates with later versions of the two test libraries using documented hooks rather than monkey-patching. This should mean that Mocha will integrate with new versions of either library without the need to release a new version of Mocha each time, which was clearly bad and unsustainable. Many thanks to @tenderlove, @zenspider & @kou for their help, suggestions & patience.
* Temporarily deprecated `require 'mocha'`. Use `require 'mocha/setup'` instead. The plan is that eventually `require 'mocha'` will *not* automatically integrate with either of the two test libraries as it does at the moment, and you'll need to explicitly & separately trigger the integration. I think this will provide a lot more flexibility and will hopefully do away with the need for the `require: false` option in the `Gemfile` which has always confused people.
* Deprecated `require 'mocha_standalone'` and `require 'mocha/standalone'`. Use `require 'mocha/api` instead.
* Although these are not part of Mocha's public API, I thought I should mention that the MiniTest and Test::Unit assertion counter classes have been combined into a single class `Mocha::Integration::AssertionCounter`.
* Extracted Mocha::Hooks module from Mocha::API and added documentation for test library authors.
* Improvements to documentation. Much of it has been combined into the README file.
* Fix #101 - Mock#respond_to? doesn't work with a string argument - thanks to @urbanautomaton.
* Fix #105 - Travis link in README - thanks to @cknadler.
* Various improvements to automated testing of integration with test libraries.
* Make deprecation warnings more prominent.

**NOTE: This release inadvertently caused deprecation warnings in some contexts:**
* When used with Rails v3.2.0-v3.2.12, v3.1.0-v3.1.10 & v3.0.0-v3.0.19.

## 0.12.7
* Officially support minitest v4.1.0 (still monkey-patching).

## 0.12.6
* Fixes #103.

## 0.12.5
* Officially support minitest v3.5.0 (still monkey-patching).

## 0.12.4
* Officially support minitest v3.4.0 & test-unit v2.5.2 (still monkey-patching).

## 0.12.3
* Revert rename of undocumented internal module since it turns out Rails/ActiveSupport is relying on its existence.

## 0.12.2
* Officially support minitest v3.3.0 (still monkey-patching)

## 0.12.1
* Deprecation warning (instead of fail fast) if neither Test::Unit nor MiniTest is loaded. Fixes #88.
* Remove deprecated access to `Mocha::Standalone`.
* Remove the deprecated file `stubba.rb`.
* Officially support test-unit v2.5.1 (still monkey-patching).
* Improve the API acceptance test.

## 0.12.0
* Fail fast if neither Test::Unit nor MiniTest is loaded. Fixes #40.
* Officially support MiniTest up to v3.2.0 (still monkey-patching).
* Officially support test-unit v2.5.0 (still monkey-patching).
* Do not monkey-patch Test::Unit or MiniTest unless we *know* it's ok.
* Add acceptance tests to demonstrate using a block as a custom parameter matcher.
* Update Travis CI build status image to use the new build under the freerange account.

## 0.11.4
* Homepage has moved to http://gofreerange.com/mocha/docs.

**WARNING: This release inadvertently included a Rails compatibility issue:**
* `TypeError: superclass mismatch for class ExpectationError` raised when using Rails v3.2.13. See #115.

## 0.11.3
* Fix for #78 i.e. alias Object#method as Object#_method, not Object#__method__ which already exists as another Ruby method.

**WARNING: This release inadvertently included a Rails compatibility issue:**
* `TypeError: superclass mismatch for class ExpectationError` raised when using Rails v3.2.13. See #115.

## 0.11.2
* Rails has a Request class which defines its own #method method. This broke the new mechanism for stubbing a method. This release includes a slightly modified version of fix #77 provided by @sikachu. See https://github.com/rails/rails/pull/5907 for further info.

**WARNING: This release inadvertently included a Rails compatibility issue:**
* `TypeError: superclass mismatch for class ExpectationError` raised when using Rails v3.2.13. See #115.

## 0.11.1
* In Ruby 1.8.7 methods accepting a block parameter were incorrectly restored without the block parameter after being stubbed. Fix for #76.

**WARNING: This release inadvertently included a Rails compatibility issue:**
* `TypeError: superclass mismatch for class ExpectationError` raised when using Rails v3.2.13. See #115.

## 0.11.0
* Store original method when stubbing rather than using alias_method. This fixes #41, #47, #74 and all tests now pass on both Ruby 1.8.7 and 1.9.3.
* Attempting to stub a method on a frozen object should fail fast. See #68.
* Prevent stubbing a method on nil by default. See #68.
* Generate documentation using YARD instead of Rdoc - removes dependency on Coderay.
* Publish documentation on Github pages instead of Rubyforge - uses rake task written by @tomafro.
* Remove agiledox which has outlived it's usefulness.
* Removed trailing whitespace throughout codebase.
* Add documentation for Mock#unstub.
* Improve documentation for ObjectMethods.
* Provide a way to run multiple tests within a single acceptance test method.

**WARNING: This release inadvertently included a significant bug - please do not use it!**

**WARNING: This release inadvertently introduced a Rails compatibility issue:**
* `TypeError: superclass mismatch for class ExpectationError` raised when using Rails v3.2.13. See #115.

## 0.10.5
* Fix for issue #66 (hopefully without regressing on issue #63) - Mocha::Mock has Mocha::Mockery as a dependency. Stop trying to pretend otherwise. Thanks to @kennyj for reporting.
* Fix a bunch of warnings in Ruby 1.9. There are still the 6 test failures mentioned in issue #41 which I suspect are due to the introspection gem not being Ruby 1.9-compatible.
* Add links to README for source code & issue tracker.
* Fix for issue #67 - Make the travis-ci badge visible in the README. Thanks to Diego Plentz for pull request.
* Fix for issue #70 - Rename Mock#expectations to Mock#__expectations__ to avoid conflicts. Thanks to Jeremy Stephens for pull request.

## 0.10.4
* Fix for issue #65 - expectations not being verified in subsequent tests.
* Fix for issue #63 - require Mocha::Mockery at Mocha::Mock class load time and not on invocation of Mock#method_missing.
* Fix for issue #45 - raise ArgumentError if Mocha::ParameterMatchers#has_entry is given
Hash with wrong number of entries.
* Make global variable name more obscure to avoid clashes with other libraries.
* Move travis-ci-related gemfiles into their own directory.

## 0.10.3
* Fix for issue #57. Gem::Requirement#=~ was only added in rubygems v1.8.0, but Object#=~ means the result of various monkey-patching checks is always false/nil for earlier versions of rubygems. However, the method it aliases #satisfied_by? has existed since Gem::Dependency was extracted from Gem::Version in rubygems v0.9.4.4, so it's much safer to use that. Thanks to fguillen for reporting and helping with diagnosis.

**WARNING: This release inadvertently included a significant bug - please do not use it!**

## 0.10.2
* Merge pull request #53. Unstubbing a method should not remove expectations for other stubbed methods. Fixes #52. Thanks to saikat.

**WARNING: This release inadvertently included a significant bug - please do not use it!**

## 0.10.1
* Merge pull request #51. Use Gem::Requirement & Gem::Version for version comparison. Fixes issue #50. Thanks to meineerde.
* Fixed typo in rdoc for Mocha::ObjectMethods.
* Improve README as suggested in issue #46. Explain that Mocha must be loaded after test libraries and how to achieve this using Bundler.
* Merge pull request #43 - nobody expects the spanish inquisition! Thanks to cairo140.
* Fix for issue #39 - improve documentation for Expectation#multiple_yields.
* Fix for issue #38 where a subtle change in test-unit v2.3.0 had been missed - only visible in verbose mode.
* Support for MiniTest up to v2.6.2 has been verified.
* Add explicit development dependency on coderay for generating syntax-highlighted code examples.

## 0.10.0
* Add Expectation#throws to allow a stubbed method to use Kernel#throw.
* Updates for versions of Test::Unit up to and including v2.3.3 (including patch by Jens Fahnenbruck).
* Updates for versions of MiniTest up to and including v2.5.1.
* Since the singleton method added by Mocha masks the underlying instance method, there's no need to move it out the way and then back again. This fixes Github issue #20, because the original method is left unchanged - https://github.com/floehopper/mocha/issues/20 (thanks to Nick Lewis).
* Handle stubbing of a singleton method, leaving the original method unchanged after the test.
* When stubbing an instance method that was originally defined as a singleton method, the original method should still exist after the test.
* Fixed mis-print in Mocha::ObjectMethods#unstub documentation (patch by Gleb Pomykalov).
* Improved test coverage around stubbing of methods defined in different ways - this makes use of the newly extracted introspection gem (although this means some tests are now failing in Ruby v1.9.2).
* Added configuration for Travis continuous integration.
* Make the gemspec the canonical reference and stop generating it from the Rakefile.
* Use the built-in Bundler rake tasks for packaging the gem.
* Use the "release" rake task provided by Bundler instead of using the Rake::XForge::Release functionality.
* Extract Object#__metaclass__ into a new metaclass gem.
* Run rake tasks without `bundle exec`.
* Avoid deprecation warning for rdoc rake task.
* Remove the `use_test_unit_gem` MOCHA_OPTION which hasn't worked since we switched to bundler - we can now run the tests specifying a different Gemfile instead.
* Use multiple Gemfiles seems to run Travis CI builds against multiple version of test-unit & minitest.

## 0.9.12
* Make Mocha's tests pass under Ruby 1.9.2 i.e. using MiniTest. One of the main issues was that we were not parsing stacktraces on MiniTest errors comprehensively enough.
* Avoid 'circular require considered harmful' warning when running Mocha's tests in Ruby 1.9.2
* Make performance tests work on Ruby 1.9.2 i.e. using MiniTest.
* Declare rake as a *development* dependency with newer versions of Rubygems since it's only needed to carry out developer-related tasks.

## 0.9.11
* Added explicit support for minitest v1.5.0 to v2.0.2.
* Make testable by rubygems-test.
* Update links to my blog and make other links consistent.
* Added a URI parameter matcher that ignores the order of query parameters so that tests can be independent of undefined hash ordering (patch by Paul Battley).
* Include unexpected invocation in failure message and change the language slightly to make the failure message less confusing. See http://floehopper.lighthouseapp.com/projects/22289/tickets/52.
* No need to create regular expression every time the BacktraceFilter#filtered method is called. See http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/66.

## 0.9.10
* Added Mocha::ObjectMethods#unstub method - https://github.com/floehopper/mocha/issues#issue/6
* Inherit Mocha::ExpectationError from Exception instead of StandardError to reduce the chances of a test passing by accident - thanks to James Sanders (jsanders) - https://github.com/floehopper/mocha/issues#issue/15
* Fixed bug - GitHub README page to link correctly to code examples - https://github.com/floehopper/mocha/issues/closed#issue/11
* Fixed bug - PASSTHROUGH_EXCEPTIONS are defined on MiniTest::Unit::TestCase not in Mocha - thanks to Brian Troutwine (blt) - https://github.com/floehopper/mocha/issues/closed#issue/14

## 0.9.9
* Avoid loading bits of the test-unit gem by accident. This is an attempt at a fix for the problem that James Adam reported [1]. By using 'load' instead of 'require' to detect the version of Test::Unit, we can avoid rubygems trying to load bits of the test-unit gem when it's not wanted. [1] http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/50#ticket-50-13
* Fix exception when running rake without test-unit gem. When test-unit gem >=v2.0.0 was installed but the "use_test_unit_gem" MOCHA_OPTIONS was not specified, a "comparison of Fixnum with Hash failed" exception was being raised when running the performance tests. This was because bits of the test-unit gem were being loaded accidentally and a Hash was being incorrectly supplied to the TestRunner.run method.
* Explicitly require rubygems for running tests via rake using test-unit gem.
* Handle newer versions of test-unit gem (v2.0.2 to v2.0.9)
* Handle newer versions of minitest gem (v1.4.0 to v1.6.0)
* Added warnings about monkey-patching test-unit and minitest to aid debugging. These are enabled by including "debug" in the MOCHA_OPTIONS environment variable. This is now a comma-separated list, so that we can specify multiple options e.g. MOCHA_OPTIONS=debug,use_test_unit_gem
* Eloy Duran (alloy) made the unit tests run on 1.9.2dev r25249.
* Eloy Duran (alloy) also improved some MiniTest TestResult code I'd written and got the acceptance tests running on Ruby 1.9 HEAD. There are still 4 failures because for some reason the backtrace line numbers are off by one. And the minitest_test test case does not run when the whole suite is run with MiniTest. These issues still need investigation.
* Fixed some acceptance tests to run in Ruby 1.9.2 - it's no longer possible to subvert the protection of a method by calling it via Object#send.
* Fixed "test:performance" rake task so it runs in Ruby 1.9.2.
* Fix test incorrectly failing under Rubinius 1.0. This test imposed too many constraints. It appears that Object#inspect legitimately calls Object#object_id in Rubinius. But we're only interested in what 'id' methods Mocha::ObjectMethods#mocha_inspect calls. By stubbing Object#inspect we can relax the constraints imposed by the test.
* Luke Redpath (lukeredpath) added new shorthand "any" and "all" composite parameter matchers using "&" and "|". This provides an alternative syntax for expecting any or all matchers to pass, e.g. foo.expects(:bar).with(equals(1) | equals(2)).
* Improved documentation for Expectation#raises. A number of people have suggested an extension to the API to cope with custom exceptions that have extra constructor parameters. However, since the arguments supplied to Expectation#raises are just passed on to Kernel#raise, it's possible to pass in an instance of an exception. Thus no change to the API is required, but it does seem worthwhile pointing this out in the docs.
* Corrected RDoc example for Expectation#never thanks to Red David (reddavis).
* Improved RDoc including a change suggested by Rohit Arondekar (rohit).
* Updated gemspec as requested by Sam Woodard (shwoodard).

## 0.9.8
* Fixed bug "NameError raised when using Mocha as a Rails plug-in" - http://floehopper.lighthouseapp.com/projects/22289/tickets/53. Since 0.9.6 the Rails plugin has been broken. See bug report for details. You will need to explicitly load Mocha *after* the test framework has been loaded, e.g. by adding "require 'mocha'" at the bottom of test/test_helper.rb.
* Make Mocha::ParameterMatchers#regexp_matches, #includes, #has_value, #has_key more robust. Thanks to Sander Hartlage.
* Allow passing a block to Mocha::Configuration methods to only change configuration for the duration of the block. Thanks to Dan Manges.
* Fixed bug "doc generation fails in 0.9.7 gem" - http://floehopper.lighthouseapp.com/projects/22289/tickets/51.
* Remove rdoc template incorporating google analytics from source control. The file just needs to exist locally and be ignored by source control. This should stop the warning showing up on e.g. RunCodeRun build results.

## 0.9.7
* Although I had provided a deprecation warning for people using Mocha::Standalone, I had assumed people wouldn't be explicitly loading the mocha/standalone.rb file. It turns out this assumption was incorrect at least in the case of Rspec. This is now fixed.

## 0.9.6
* Version 2.0.1 of the test-unit gem introduced a private 'run_test' method on TestCase which clashed with the public TestRunner#run_test method. So this latter method has been renamed to 'run_as_test'.
* Stop requiring rubygems - this should be an environmental choice for the user. http://gist.github.com/54177 - describes why requiring rubygems in your library code is a bad idea.
* It seems like overkill to vendorize coderay and meta_project when they're only needed to generate the examples for documentation and for publishing files on RubyForge. So I'm removing them and installing them locally as gems when I need them.
* Added support for 'test-unit' gem (version >= 2.0). Note that as with other versions of Test::Unit I'm completely replacing the TestCase#run method. Unfortunately in version 2.0.0 this method differs slightly from the same method in version 2.0.1 & 2.0.2, so we have to provide different implementations to ensure that the internal working of Test::Unit are not compromised by Mocha. Note also that unless the 'test-unit' gem is loaded, requiring 'test/unit' leads to a mixture of stdlib and gem classes being loaded causing errors. To avoid a dependency on rubygems, the gem is loaded only if MOCHA_OPTIONS is set to 'use_test_unit_gem' - this option is only intended for use in running Mocha's own tests. It might be worthwhile to create a shim gem like minitest_tu_shim to allow the test-unit gem to completely replace the stdlib, but that's a job for another day. The changes in the Rakefile are to make the default task run with the 'test-unit' gem (version >= 2.0).
* Renamed Mocha::Standalone to Mocha::API to better reflect its purpose. Added a deprecation warning for those who are referencing Mocha::Standalone.
* Fix exception raised by HasEntry#matches? if first param is not a Hash (thanks to Taylor Barstow).
* Ken Collins reported [1] that Mocha is always loading MiniTest if it is available and loading it causes some Rails/ActionPack tests to break. I've removed the loading of MiniTest, but this now means the user has to ensure that if they want to use MiniTest in conjunction with Mocha, he must load MiniTest before loading Mocha. [1] http://rails.lighthouseapp.com/projects/8994-ruby-on-rails/tickets/2060
* Implemented Bacon integration (thanks to Ubiratan Pires Alberton), but this was then removed after deciding only to maintain integration with Test::Unit and MiniTest which are both Ruby standard libraries. See mailing list for details.
* Don't monkey-patch MiniTest if it's already been monkey-patched by Mocha.
* Fixed bug: MiniTest integration was counting ExpectationErrors as errors not failures. http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/41.
* Fixed bug: Some Bacon tests were failing in Ruby 1.9.1. http://floehopper.lighthouseapp.com/projects/22289-mocha/tickets/43.
* Chad Humphries pointed out that in Ruby 1.9.1, if you are not using Test::Unit or MiniTest, Mocha will attempt to load and monkey-patch Test::Unit. Mocha will now only monkey-patch Test::Unit and/or MiniTest if they have already been loaded. MiniTest tests will now run in both Ruby 1.8.6 (with MiniTest gem) and in Ruby 1.9.1 (with MiniTest std lib). See Ligthouse ticket - http://floehopper.lighthouseapp.com/projects/22289/tickets/49.
* Made Mocha compatible with minitest 1.4.0 and above (thanks to Denis Defreyne).

## 0.9.5
* Fixed Lighthouse bug #32 - stub_everything should mean mock responds to anything.
* Added Expectation#twice to improve readability. Thanks to pull request from Celestino Gomes.
* In Ruby 1.9.1, requiring 'test/unit' loads a thin wrapper around MiniTest and Test::Unit::TestCase ends up inheriting from MiniTest::Unit::TestCase. So we need to avoid including the Mocha modules more than once to avoid nasty consequences. Thanks to Matthias Hennemeyer for help with this.
* Ruby 1.9 includes rake, but not rake/contrib. For the moment I've moved the sshpublisher require into the only rake task that needs it, so that I can at least run the tests in Ruby 1.9. It looks like I will need to build a rake/contrib gem or similar to get this working properly - http://intertwingly.net/blog/2008/01/07/Rake-Contrib-for-1-9

## 0.9.4
* Added mocha.gemspec file generated with Chad Woolley's new rake task, so that a floehopper-mocha gem will get built on GitHub.
* Add rake task to update mocha.gemspec with unique version, which will cause gem to be auto-built on github
* As Tobias Crawley correctly pointed out in feature request #23055 "stubs(with_hash) not working with existing object" [1], following the principle of least surprise, it should be possible to call ObjectMethods#expects & ObjectMethods#stubs with a Hash of method_names vs return_values like you can with Mock#expects & Mock#stubs. I've also updated & improved the docs to reflect the changes. [1] http://rubyforge.org/tracker/index.php?func=detail&aid=23055&group_id=1917&atid=7480
* Removed deprecated gem autorequire.

## 0.9.3
* Added support for MiniTest thanks to Jeff Smick.
* Fixed a possible bug with some of the non-default Configuration options relating to the argument to Object#respond_to?
* As per Jay Fields recommendations [1] and with further impetus from a talk at Ruby Manor, any methods added to core classes are now added by including a module. This means that Mocha is a better citizen of the Ruby world and it's behaviour is more easily extended. [1] http://blog.jayfields.com/2008/07/ruby-underuse-of-modules.html & http://blog.jayfields.com/2008/07/ruby-redefine-method-behavior.html
* Removed deprecated gem autorequire.

## 0.9.2
* Improved documentation to address [#22530] 'Mock methods with multiple return values not possible?'
* respond_with parameter matcher was not available in tests.
* Patch [#22630] Fix for a bug in running Rails tests with Ruby 1.8.7. Array#flatten was being called which in turn was checking whether each element responded to #to_ary. This check was using the two parameter version of #respond_to?, but Mock was only defining a one parameter version.

## 0.9.1

* Fixed bug #21465 - expects & stubs should support method names as strings (as well as symbols) or fail fast. Convert all expectation method names to a symbol in case they were supplied as a string.
* By removing Mock#unexpected_method_called we reduce the number of methods vulnerable to the problem that surfaced in bug #21563.
* Fix bug #21563 - stubbing 'verified?' method is unsafe. Instance method names on the Mock class should be more obscure.
* Performance improvement. StubbaExampleTest goes twice as fast on my local machine.
* Added primitive performance test to default rake task.
* Fix format of case statements which don't work in Ruby 1.9 and make others consistent.
* There is no point in running (potentially expensive) checks if configuration is set to allow such checks to fail. This is a relatively quick fix in response to Chris McGrath's performance problems.
* Fix for bug #21161 - 'uninitialized constant Deprecation in stubba.rb'.
* It's more readable to talk about 'once' and 'twice' rather than '1 time' and '2 times'.
* Fix bug #20883 - never should raise when called to prevent follow up errors. Fail fast when there are no matching invokable expectations and handle the stub_everything case sensibly. This might not be entirely backwards compatible, but I think the benefits outweigh the risks. The most likely change is that a test that was already failing will now fail faster, which doesn't seem so awful.

## 0.9.0

* Configurable warnings or errors
  * when a method on a non-public method is stubbed
  * when a method on a non-existent method is stubbed
  * when a method on a non-mock object is stubbed
  * when a method is stubbed unnecessarily (i.e. the stubbed method is not called during the test)

* Improved error messages
  * User-friendly list of unsatisfied expectations, satisfied expectations and state machines.
  * Improved readability of cardinality description.
  * Display sensible failure message for any_instance expectations e.g. "#<AnyInstance:Foo>.bar - expected calls: 1, actual calls: 0"

* Parameter matchers
  * New to this release
    * optionally (allows matching of optional parameters if available)
    * yaml_equivalent (allows matching of YAML that represents the specified object)
    * responds_with (tests the quack not the duck)
  * Nesting of parameter matchers is now supported.

* Optional block passed into mock initializer is evaluated in the context of the new mock instance and can be used as a shortcut to set up expectations.

* Added JMock-style sequences for constraining the order of expected invocations. See Standalone#sequence and Expectation#in_sequence.

* Added JMock-style states for constraining the order of expected invocations. See Standalone#states, Expectation#then, Expectation#when and StateMachine.

* Compatibility with versions of Ruby
  * Compatibility with Ruby v1.9. All test errors and warnings fixed.
  * Nasty fix so that TestCaseAdaptor works consistently with earlier versions of Test::Unit as well as more recent versions.
  * Added platform to gem specification to avoid bug in rubygems 0.9.5 - see http://www.dcmanges.com/blog/rubygems-0-9-5-platform-bug and http://rubygems.org/read/chapter/20#platform.
  * Make ExpectationRaiser deal with subclasses of Interrupt which seem to need a message supplied in the raise statement in Ruby 1.8.6 (but not 1.8.4 or 1.9). Not sure this is really Mocha's responsibility.

* Added deprecation warning in stubba.rb which is no longer needed and will be removed.

* Supply positioning information to evals to improve any error messages. See http://ola-bini.blogspot.com/2008/01/ruby-antipattern-using-eval-without.html

* Bug fixes
  * 18914 in revision 296 - http://rubyforge.org/tracker/index.php?func=detail&aid=18914&group_id=1917&atid=7477
  * 18917 in revision 295 - http://rubyforge.org/tracker/index.php?func=detail&aid=18917&group_id=1917&atid=7477
  * 18336 in revision 287 - http://rubyforge.org/tracker/index.php?func=detail&aid=18336&group_id=1917&atid=7477
  * 17835 in revision 255 - http://rubyforge.org/tracker/index.php?func=detail&aid=17835&group_id=1917&atid=7477
  * 17412 in revision 242 - http://rubyforge.org/tracker/index.php?func=detail&aid=17412&group_id=1917&atid=7477
  * 15977 in revision 198 - http://rubyforge.org/tracker/index.php?func=detail&aid=15977&group_id=1917&atid=7477
  * 11885 in revision 156 - http://rubyforge.org/tracker/index.php?func=detail&aid=11885&group_id=1917&atid=7477

## 0.5.5

- Renamed Matches parameter matcher to RegexpMatches for clarity.
- Added noframes tag to rdoc index to assist Google.

## 0.5.4

- Added matches parameter matcher for matching regular expressions.

## 0.5.3

- Attempt to fix packaging problems by switching to newer version (1.15.1) of gnutar and setting COPY_EXTENDED_ATTRIBUTES_DISABLE environment variable.
- Removed unused ExpectationSequenceError exception.
- Added instance_of and kind_of parameter matchers.
- Added Google Webmaster meta tag to rdoc template header.
- Put Google Webmaster meta tag in the right header i.e. the one for the index page.

## 0.5.2

- Fix bug 11885 - "never doesn't work with stub_everything" submitted by Alexander Lang. In fixing this bug, also fixed undiscoverd bug where expected & actual invocation counts were being incorrectly reported which seems to have been introduced when fixes were added for invocation dispatch (see MockedMethodDispatchAcceptanceTest).
- Previously when an expectation did not allow more invocations, it was treated as not matching. Now we prefer matching expectations which allow more invocations, but still match expectations which cannot allow more invocations. I think this may be overcomplicating things, but let's see how it goes.

## 0.5.1

- Fixed bug #11583 "Mocha 0.5.0 throwing unexpected warnings". Also switched on ruby warning for all rake test tasks. Fixed majority of warnings, but some left to fix.

## 0.5.0

- Parameter Matchers - I've added a few Hamcrest-style parameter matchers which are designed to be used inside Expectation#with. The following matchers are currently available: anything(), includes(), has_key(), has_value(), has_entry(), all_of() & any_of(). More to follow soon. The idea is eventually to get rid of the nasty parameter_block option on Expectation#with.

  object = mock()
  object.expects(:method).with(has_key('key_1'))
  object.method('key_1' => 1, 'key_2' => 2)
  # no verification error raised

  object = mock()
  object.expects(:method).with(has_key('key_1'))
  object.method('key_2' => 2)
  # verification error raised, because method was not called with Hash containing key: 'key_1'

- Values Returned and Exceptions Raised on Consecutive Invocations - Allow multiple calls to Expectation#returns and Expectation#raises to build up a sequence of responses to invocations on the mock. Added syntactic sugar method Expectation#then to allow more readable expectations.

  object = mock()
  object.stubs(:method).returns(1, 2).then.raises(Exception).then.returns(4)
  object.method # => 1
  object.method # => 2
  object.method # => raises exception of class Exception
  object.method # => 4

- Yields on Consecutive Invocations - Allow multiple calls to yields on single expectation to allow yield parameters to be specified for consecutive invocations.

  object = mock()
  object.stubs(:method).yields(1, 2).then.yields(3)
  object.method { |*values| p values } # => [1, 2]
  object.method { |*values| p values } # => [3]

- Multiple Yields on Single Invocation - Added Expectation#multiple_yields to allow a mocked or stubbed method to yield multiple times for a single invocation.

  object = mock()
  object.stubs(:method).multiple_yields([1, 2], [3])
  object.method { |*values| p values } # => [1, 2] # => [3]

- Invocation Dispatch - Expectations were already being matched in reverse order i.e. the most recently defined one was being found first. This is still the case, but we now stop matching an expectation when its maximum number of expected invocations is reached. c.f. JMock v1. A stub will never stop matching by default. Hopefully this means we can soon get rid of the need to pass a Proc to Expectation#returns.

  object = mock()
  object.stubs(:method).returns(2)
  object.expects(:method).once.returns(1)
  object.method # => 1
  object.method # => 2
  object.method # => 2
  # no verification error raised

  # The following should still work...

  Time.stubs(:now).returns(Time.parse('Mon Jan 01 00:00:00 UTC 2007'))
  Time.now # => Mon Jan 01 00:00:00 UTC 2007
  Time.stubs(:now).returns(Time.parse('Thu Feb 01 00:00:00 UTC 2007'))
  Time.now # => Thu Feb 01 00:00:00 UTC 2007

- Deprecate passing an instance of Proc to Expectation#returns.
- Explicitly include all Rakefile dependencies in project.
- Fixed old Stubba example.
- Fix so that it is possible for a stubbed method to raise an Interrupt exception without a message in Ruby 1.8.6
- Added responds_like and quacks_like.
- Capture standard object methods before Mocha adds any.
- Added Expectation#once method to make interface less surprising.
- Use Rake::TestTask to run tests. Created three separate tasks to run unit, integration & acceptance tests. Split inspect_test into one file per TestCase. Deleted superfluous all_tests file.
- Fiddled with mocha_inspect and tests to give more sensible results on x86 platform.
- Fixed bug #7834 "infinite_range.rb makes incorrect assumption about to_f" logged by James Moore.

## 0.4.0

- Allow naming of mocks (patch from Chris Roos).
- Specify multiple return values for consecutive calls.
- Improved consistency of expectation error messages.
- Allow mocking of Object instance methods e.g. kind_of?, type.
- Provide aliased versions of #expects and #stubs to allow mocking of these methods.
- Added at_least, at_most, at_most_once methods to expectation.
- Allow expects and stubs to take a hash of method and return values.
- Eliminate warning: "instance variable @yield not initialized" (patch from Xavier Shay).
- Restore instance methods on partial mocks (patch from Chris Roos).
- Allow stubbing of a method with non-word characters in its name (patch from Paul Battley).
- Removed coupling to Test::Unit.
- Allow specified exception instance to be raised (patch from Chris Roos).
- Make mock object_id appear in hex like normal Ruby inspect (patch from Paul Battley).
- Fix path to object.rb in rdoc rake task (patch from Tomas Pospisek).
- Reverse order in which expectations are matched, so that last expectation is matched first. This allows e.g. a call to #stubs to be effectively overridden by a call to #expects (patch from Tobias Lutke).
- Stubba & SmartTestCase modules incorporated into Mocha module so only need to require 'mocha' - no longer need to require 'stubba'.
- AutoMocha removed.

## 0.3.3

- Quick bug fix to restore instance methods on partial mocks (for Kevin Clark).

## 0.3.2

- Examples added.

## 0.3.1

- Dual licensing with MIT license added.

## 0.3.0

* Rails plugin.
* Auto-verify for expectations on concrete classes.
* Include each expectation verification in the test result assertion count.
* Filter out noise from assertion backtraces.
* Point assertion backtrace to line where failing expectation was created.
* New yields method for expectations.
* Create stubs which stub all method calls.
* Mocks now respond_to? expected methods.

## 0.2.1

* Rename MochaAcceptanceTest::Rover#move method to avoid conflict with Rake (in Ruby 1.8.4 only?)

## 0.2.0

* Small change to SetupAndTeardown#teardown_stubs suggested by Luke Redpath (http://www.lukeredpath.co.uk) to allow use of Stubba with RSpec (http://rspec.rubyforge.org).
* Reorganized directory structure and extracted addition of setup and teardown methods into SmartTestCase mini-library.
* Addition of auto-verify for Mocha (but not Stubba). This means there is more significance in the choice of expects or stubs in that any expects on a mock will automatically get verified.

So instead of...

  wotsit = Mocha.new
  wotsit.expects(:thingummy).with(5).returns(10)
  doobrey = Doobrey.new(wotsit)
  doobrey.hoojamaflip
  wotsit.verify

you need to do...

  wotsit = mock()
  wotsit.expects(:thingummy).with(5).returns(10)
  doobrey = Doobrey.new(wotsit)
  doobrey.hoojamaflip
  # no need to verify

There are also shortcuts as follows...

instead of...

  wotsit = Mocha.new
  wotsit.expects(:thingummy).returns(10)
  wotsit.expects(:summat).returns(25)

you can have...

  wotsit = mock(:thingummy => 5, :summat => 25)

and instead of...

  wotsit = Mocha.new
  wotsit.stubs(:thingummy).returns(10)
  wotsit.stubs(:summat).returns(25)

you can have...

  wotsit = stub(:thingummy => 5, :summat => 25)

## 0.1.2

* Minor tweaks

## 0.1.1

* Initial release.

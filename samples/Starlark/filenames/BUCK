include_defs('//tools/build.defs')

gerrit_war(name = 'gerrit')
gerrit_war(name = 'gwtgerrit',   ui = 'ui_dbg')
gerrit_war(name = 'headless',    ui = None)
gerrit_war(name = 'chrome',      ui = 'ui_chrome')
gerrit_war(name = 'firefox',     ui = 'ui_firefox')
gerrit_war(name = 'safari',      ui = 'ui_safari')
gerrit_war(name = 'polygerrit',  ui = 'polygerrit')
gerrit_war(name = 'withdocs', docs = True)
gerrit_war(name = 'release',  ui = 'ui_optdbg_r', docs = True, context = ['//plugins:core'],  visibility = ['//tools/maven:'])

API_DEPS = [
  '//gerrit-acceptance-framework:acceptance-framework',
  '//gerrit-acceptance-framework:acceptance-framework-src',
  '//gerrit-acceptance-framework:acceptance-framework-javadoc',
  '//gerrit-extension-api:extension-api',
  '//gerrit-extension-api:extension-api-src',
  '//gerrit-extension-api:extension-api-javadoc',
  '//gerrit-plugin-api:plugin-api',
  '//gerrit-plugin-api:plugin-api-src',
  '//gerrit-plugin-api:plugin-api-javadoc',
  '//gerrit-plugin-gwtui:gwtui-api',
  '//gerrit-plugin-gwtui:gwtui-api-src',
  '//gerrit-plugin-gwtui:gwtui-api-javadoc',
]

zip_file(
  name = 'api',
  srcs = API_DEPS,
)

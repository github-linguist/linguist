#!/usr/bin/env node
import { satisfies } from 'semver';
import { red } from './lib/Helper/Logging';

import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';

const NODE_VERSION_RANGE = '>=18.20.0';

// Have to run this above the other imports because they are importing clack that
// has the problematic imports.
if (!satisfies(process.version, NODE_VERSION_RANGE)) {
  red(
    `Sentry wizard requires Node.js ${NODE_VERSION_RANGE}. You are using Node.js ${process.version}. Please upgrade your Node.js version.`,
  );
  process.exit(1);
}

import { Integration, Platform } from './lib/Constants';
import { run } from './src/run';
import { WIZARD_VERSION } from './src/version';

export * from './lib/Setup';

const PRESELECTED_PROJECT_OPTIONS: Record<string, yargs.Options> = {
  'preSelectedProject.authToken': {
    describe: 'Preselected project auth token',
    hidden: true,
  },
  'preSelectedProject.selfHosted': {
    describe: 'Preselected project is self-hosted',
    hidden: true,
  },
  'preSelectedProject.dsn': {
    describe: 'Preselected project DSN',
    hidden: true,
  },
  'preSelectedProject.id': {
    describe: 'Preselected project id',
    hidden: true,
  },
  'preSelectedProject.projectSlug': {
    describe: 'Preselected project slug',
    hidden: true,
  },
  'preSelectedProject.projectName': {
    describe: 'Preselected project name',
    hidden: true,
  },
  'preSelectedProject.orgId': {
    describe: 'Preselected organization id',
    hidden: true,
  },
  'preSelectedProject.orgName': {
    describe: 'Preselected organization name',
    hidden: true,
  },
  'preSelectedProject.orgSlug': {
    describe: 'Preselected organization slug',
    hidden: true,
  },
};
const xcodeProjectDirOption: yargs.Options = {
  default: undefined,
  describe:
    'Path to the project containing the Xcode project file. Only applies to the Apple wizard.',
  type: 'string',
  // This is a hidden option because it is used as an internal option
  hidden: true,
};

// eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
const argv = yargs(hideBin(process.argv), process.cwd())
  .options({
    debug: {
      default: false,
      describe: 'Enable verbose logging\nenv: SENTRY_WIZARD_DEBUG',
      type: 'boolean',
    },
    uninstall: {
      default: false,
      describe: 'Revert project setup process\nenv: SENTRY_WIZARD_UNINSTALL',
      type: 'boolean',
    },
    'skip-connect': {
      default: false,
      describe:
        'Skips the connection to the server\nenv: SENTRY_WIZARD_SKIP_CONNECT',
      type: 'boolean',
    },
    quiet: {
      default: false,
      describe:
        'Do not fallback to prompting user asking questions\nenv: SENTRY_WIZARD_QUIET',
      type: 'boolean',
    },
    i: {
      alias: 'integration',
      choices: Object.keys(Integration),
      describe:
        'Choose the integration to setup\nenv: SENTRY_WIZARD_INTEGRATION',
    },
    p: {
      alias: 'platform',
      choices: Object.keys(Platform),
      describe: 'Choose platform(s)\nenv: SENTRY_WIZARD_PLATFORM',
      type: 'array',
    },
    u: {
      alias: 'url',
      describe: 'The url to your Sentry installation\nenv: SENTRY_WIZARD_URL',
    },
    project: {
      type: 'string',
      describe: 'The Sentry project slug to use',
      defaultDescription: 'Select project during setup',
      default: undefined,
    },
    org: {
      type: 'string',
      describe: 'The Sentry org slug to use',
      defaultDescription: 'Select org during setup',
      default: undefined,
    },
    saas: {
      default: false,
      describe: 'Skip the self-hosted or SaaS URL selection process',
      defaultDescription: 'Select self-hosted or SaaS during setup',
      type: 'boolean',
    },
    s: {
      alias: 'signup',
      default: false,
      describe: 'Redirect to signup page if not logged in',
      type: 'boolean',
    },
    'disable-telemetry': {
      default: false,
      describe: "Don't send telemetry data to Sentry",
      type: 'boolean',
    },
    'promo-code': {
      alias: 'promo-code',
      describe: 'A promo code that will be applied during signup',
      type: 'string',
    },
    'force-install': {
      default: false,
      describe: 'Force install the SDK NPM package',
      type: 'boolean',
    },
    'ignore-git-changes': {
      default: false,
      describe: 'Ignore git changes in the project',
      type: 'boolean',
    },
    spotlight: {
      default: false,
      describe:
        'Enable Spotlight for local development. This does not require a Sentry account or project.',
      type: 'boolean',
    },
    'xcode-project-dir': xcodeProjectDirOption,
    ...PRESELECTED_PROJECT_OPTIONS,
  })
  // This prevents `yargs` from trying to read the local package.json
  // as it's not available in the Node Single Executable Binary artifacts versions
  .version(WIZARD_VERSION).argv;

// @ts-expect-error - for some reason TS doesn't recognize the aliases as valid properties
// meaning it only knows e.g. u but not url. Maybe a bug in this old version of yargs?
// Can't upgrade yargs though without dropping support for Node 14.
void run(argv);

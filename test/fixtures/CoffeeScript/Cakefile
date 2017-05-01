http = require 'http'
https = require 'https'
fs = require 'fs'
path = require 'path'
{spawn, exec} = require 'child_process'
semver = require 'semver'
AdmZip = require('adm-zip')
GitHubApi = require 'github'

github = new GitHubApi(version: '3.0.0')

# ----------------
# Server / Builder
# ----------------

option '-P', '--production', 'run server in production mode'
option null, '--port [PORT]', 'listen on specified port (default 3333)'

LOCAL_BRUNCH = path.join('.', 'node_modules', '.bin', 'brunch')

spawnBrunch = (flags, env) ->
  if fs.existsSync(LOCAL_BRUNCH)
    brunch = spawn LOCAL_BRUNCH, flags, env
  else
    console.error 'Warning, using global brunch. Run `npm install`.'
    brunch = spawn 'brunch', flags, env

  brunch.stdout.on 'data', (data) -> console.log data.toString().trim()
  brunch.stderr.on 'data', (data) -> console.log data.toString().trim()

runBrunchWatch = (options, shouldStartServer) ->
  flags = ['w']
  flags.push '-s' if shouldStartServer

  if options.production?
    flags.push('-P')
    process.env.BRUNCH_ENV = 'production'

  if options.port?
    flags.push '-p'
    flags.push options.port

  spawnBrunch flags, process.env

task 'server', 'start the brunch server in development', (options) ->
  runBrunchWatch(options, true)

task 'watch', 'build the app continuously without a server', (options) ->
  runBrunchWatch(options, false)

task 'build', 'build for production', ->
  process.env.BRUNCH_ENV = 'production'
  spawnBrunch ['b', '-P'], process.env

task 'test', 'run brunch in the test environment', ->
  flags = ['w', '-s']
  process.env.BRUNCH_ENV = 'test'
  spawnBrunch flags, process.env

# -------------
# Tapas Updates
# -------------
updateMessage = 'update Tapas to latest (Cakefile, package.json, portkey.json,
 config.coffee, generators/*)'
task 'tapas:update', updateMessage, (options) ->
  url = 'https://codeload.github.com/mutewinter/tapas-with-ember/zip/master'
  filesToUpdate = [
    'Cakefile'
    'package.json'
    'portkey.json'
    'config.coffee'
    'generators/'
    'testem.json'
    'bower.json'
  ]
  https.get url, (res) ->
    data = []
    dataLen = 0

    res.on('data', (chunk) ->
      data.push(chunk)
      dataLen += chunk.length
    ).on('end', ->
      buf = new Buffer(dataLen)

      pos = 0
      for dataItem in data
        dataItem.copy(buf, pos)
        pos += dataItem.length

      zip = new AdmZip(buf)

      filesToUpdate.forEach (file) ->
        targetFile = "tapas-with-ember-master/#{file}"
        if /\/$/.test(file)
          zip.extractEntryTo(targetFile, file, false, true)
        else
          zip.extractEntryTo(targetFile, '', false, true)
    )

# --------------
# Script Updates
# --------------

EMBER_BASE_URL = 'http://builds.emberjs.com'
GITHUB_API_URL = 'https://api.github.com'
EMBER = {}
EMBER_DATA = {}
['release', 'beta', 'canary'].forEach (build) ->
  EMBER[build] =
    prod: "#{EMBER_BASE_URL}/#{build}/ember.prod.js"
    dev: "#{EMBER_BASE_URL}/#{build}/ember.js"
  EMBER_DATA[build] =
    prod: "#{EMBER_BASE_URL}/#{build}/ember-data.prod.js"
    dev: "#{EMBER_BASE_URL}/#{build}/ember-data.js"

EMBER['tag'] =
  prod: "#{EMBER_BASE_URL}/tags/{{tag}}/ember.prod.js"
  dev: "#{EMBER_BASE_URL}/tags/{{tag}}/ember.js"

EMBER_DATA['tag'] =
  prod: "#{EMBER_BASE_URL}/tags/{{tag}}/ember-data.prod.js"
  dev: "#{EMBER_BASE_URL}/tags/{{tag}}/ember-data.js"

downloadFile = (src, dest) ->
  console.log('Downloading ' + src + ' to ' + dest)
  data = ''
  request = http.get src, (response) ->
    response.on('data', (chunk) ->
      data += chunk
    )
    response.on('end', ->
      fs.writeFileSync(dest, data)
    )

downloadEmberFile = (src, dest) ->
  downloadFile(src, "vendor/ember/#{dest}")

listTags = (user, repo, since, name, command) ->
  github.repos.getTags(user: user, repo: repo, (resp, tags) ->
    for tag in tags
      if semver.valid(tag.name) and !semver.lt(tag.name, since)
        firstTag = tag.name unless firstTag
        console.log "  #{tag.name}"
    console.log "Install with cake -t \"#{firstTag}\" #{command}"
  )

installEmberFiles = (project, filename, options) ->
  if 'tag' of options
    # Download a Tag
    tag = options.tag
    tag = "v#{tag}" unless /^v/.test(tag)
    downloadEmberFile(project['tag'].dev.replace(/{{tag}}/, tag),
      "development/#{filename}")
    downloadEmberFile(project['tag'].prod.replace(/{{tag}}/, tag),
      "production/#{filename}")
  else
    # Download a Channel
    channel = options.channel ? 'release'
    downloadEmberFile project[channel].dev, "development/#{filename}"
    downloadEmberFile project[channel].prod, "production/#{filename}"

# Channel
option '-c', '--channel "[CHANNEL_NAME]"',
  'relase, beta, or canary (http://emberjs.com/builds)'

# Tag
option '-t', '--tag "[TAG_NAME]"',
  'a tagged release to install. Run cake ember:list to see known tags'

# -----
# Ember
# -----
task 'ember:install', 'install latest Ember', (options) ->
  installEmberFiles(EMBER, 'ember.js', options)

task 'ember:list', 'list tagged relases of Ember since v1.0.0', (options) ->
  listTags 'emberjs', 'ember.js', 'v1.0.0', 'Ember', 'ember:install'

# ----------
# Ember Data
# ----------
task 'ember-data:install', 'install latest Ember Data', (options) ->
  options.channel or= 'beta'
  installEmberFiles(EMBER_DATA, 'ember-data.js', options)

task 'ember-data:list', 'list tagged relases of Ember Data', (options) ->
  listTags 'emberjs', 'data', 'v0.0.1', 'Ember Data',
    'ember-data:install'

# -----------
# Ember Model
# -----------
EMBER_MODEL =
  dev: 'http://builds.erikbryn.com/ember-model/ember-model-latest.js'
  prod: 'http://builds.erikbryn.com/ember-model/ember-model-latest.prod.js'

task 'ember-model:install', 'install latest Ember Model', (options) ->
  downloadEmberFile EMBER_MODEL.dev, 'development/ember-model.js'
  downloadEmberFile EMBER_MODEL.prod, 'production/ember-model.js'

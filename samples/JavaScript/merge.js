#!/usr/bin/env osascript -l JavaScript

function run(argv) {
  var app = Application.currentApplication()
  app.includeStandardAdditions = true

  var word = Application('Microsoft Word')

  app.doShellScript('mkdir -p /tmp/word_git')
  app.doShellScript('cp "' + argv[0] + '" /tmp/word_git/base.docx')
  app.doShellScript('cp "' + argv[1] + '" /tmp/word_git/local.docx')
  app.doShellScript('cp "' + argv[2] + '" /tmp/word_git/remote.docx')

  word.open('/tmp/word_git/local.docx', {addToRecentFiles: false})
  word.documents['local.docx'].close()
  word.open('/tmp/word_git/remote.docx', {addToRecentFiles: false})
  word.documents['remote.docx'].close()

  word.open('/tmp/word_git/base.docx', {addToRecentFiles: false})
  word.documents['base.docx'].compare({path: '/tmp/word_git/local.docx', authorName: "Local"})
  word.documents[0].saveAs({fileName: '/tmp/word_git/local_comp.docx'})
  word.documents['local_comp.docx'].close()

  word.documents['base.docx'].close()
  word.open('/tmp/word_git/base.docx', {addToRecentFiles: false})
  word.documents['base.docx'].compare({path: '/tmp/word_git/remote.docx', authorName: "Remote"})
  word.documents[0].saveAs({fileName: '/tmp/word_git/remote_comp.docx'})
  word.documents['remote_comp.docx'].close()
  
  word.documents['base.docx'].close({saving: "no"})
  
  word.open('/tmp/word_git/local_comp.docx', {addToRecentFiles: false})
  Application("Microsoft Word").merge(Application("Microsoft Word").documents.byName("local_comp.docx"), {fileName:"/tmp/word_git/remote_comp.docx"})
  word.documents[0].saveAs({fileName: '/tmp/word_git/merged.docx'})
  word.documents['local_comp.docx'].close({saving: "no"})
  
  app.displayDialog('Merge your changes now.', {buttons: ["Done Merging"]})
  word.documents['merged.docx'].close({saving: "yes"})

  app.doShellScript('cp /tmp/word_git/remote.docx "' + argv[3] + '"')
}

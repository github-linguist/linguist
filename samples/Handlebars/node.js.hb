var exec = require('child_process').exec;

exec('{{escapeBackslashes command}}{{#if task}} {{escapeBackslashes task}}{{/if}}{{#if args}} {{{escapeBackslashes args}}}{{/if}}', {
       cwd: '{{escapeBackslashes gruntfileDirectory}}'
     }, function (err, stdout, stderr) {

  var exitCode = 0;
  if (err) {
    console.log(stderr || err);
    exitCode = -1;
  }{{#unless preventExit}}

  process.exit(exitCode);{{/unless}}
}).stdout.on('data', function (chunk){
    process.stdout.write(chunk);
});
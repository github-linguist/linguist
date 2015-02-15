var keypress = require('keypress');

keypress(process.stdin);

process.stdin.on('keypress', function (ch, key) {
    if (key && (key.name === 'y' || key.name === 'n')) {
       var reply = key.name === 'y';
       console.log('Reply:', reply);
       // ...do something with 'reply'...
    }
});

process.stdin.setRawMode(true);
process.stdin.resume();

var w = parseInt( get_input("Enter a width:") );
var w = parseInt( get_input("Enter a height:") );

// create the 2-D array
var a = new Array(h);
for (var i = 0; i < h; i++)
  a[i] = new Array(w);

a[0][0] = 'foo';
WScript.Echo('a[0][0] = ' + a[0][0]);

a = null;

function get_input(prompt) {
    output(prompt);
    try {
        return WScript.StdIn.readLine();
    } catch(e) {
        return readline();
    }
}
function output(prompt) {
    try {
        return WScript.echo(prompt);
    } catch(e) {
        return print(prompt);
    }
}

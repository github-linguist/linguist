function evalArithmeticExp(s) {
  s = s.replace(/\s/g,'').replace(/^\+/,'');
  var rePara = /\([^\(\)]*\)/;
  var exp = s.match(rePara);

  while (exp = s.match(rePara)) {
    s = s.replace(exp[0], evalExp(exp[0]));
  }
  return evalExp(s);

  function evalExp(s) {
    s = s.replace(/[\(\)]/g,'');
    var reMD = /\d+\.?\d*\s*[\*\/]\s*[+-]?\d+\.?\d*/;
    var reM = /\*/;
    var reAS = /-?\d+\.?\d*\s*[\+-]\s*[+-]?\d+\.?\d*/;
    var reA  = /\d\+/;
    var exp;

    while (exp = s.match(reMD)) {
      s = exp[0].match(reM)? s.replace(exp[0], multiply(exp[0])) : s.replace(exp[0], divide(exp[0]));
    }

    while (exp = s.match(reAS)) {
      s = exp[0].match(reA)? s.replace(exp[0], add(exp[0])) : s.replace(exp[0], subtract(exp[0]));
    }

    return '' + s;

    function multiply(s, b) {
      b = s.split('*');
      return b[0] * b[1];
    }

    function divide(s, b) {
      b = s.split('/');
      return b[0] / b[1];
    }

    function add(s, b) {
      s = s.replace(/^\+/,'').replace(/\++/,'+');
      b = s.split('+');
      return Number(b[0]) + Number(b[1]);
    }

    function subtract(s, b) {
      s = s.replace(/\+-|-\+/g,'-');

      if (s.match(/--/)) {
        return add(s.replace(/--/,'+'));
      }
      b = s.split('-');
      return b.length == 3? -1 * b[1] - b[2] : b[0] - b[1];
    }
  }
}

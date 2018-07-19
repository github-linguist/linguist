import std.stdio, std.string, std.ascii, std.conv, std.array,
       std.exception, std.traits;

struct Stack(T) {
    T[] data;
    alias data this;
    void push(T top) pure nothrow { data ~= top; }

    T pop(bool discard = true)() pure {
      if (data.empty)
        throw new Exception("Stack Empty");
      auto top = data.back;
      static if (discard)
        data.popBack;
      return top;
    }
}

enum Type {         Num, OBkt, CBkt, Add, Sub, Mul, Div }
immutable opChar = ["#", "(",  ")",  "+", "-", "*", "/"];
immutable opPrec = [ 0,  -9,   -9,    1,   1,   2,   2];

abstract class Visitor { void visit(XP e); }

final class XP {
  immutable Type type;
  immutable string str;
  immutable int pos; // Optional, to dispaly AST struct.
  XP LHS, RHS;

  this(string s=")", int p = -1) nothrow {
    str = s;
    pos = p;
    auto localType = Type.Num;
    foreach_reverse (immutable t; [EnumMembers!Type[1 .. $]])
      if (opChar[t] == s)
        localType = t;
    this.type = localType;
  }

  override int opCmp(Object other) pure {
    auto rhs = cast(XP)other;
    enforce(rhs !is null);
    return opPrec[type] - opPrec[rhs.type];
  }

  void accept(Visitor v) { v.visit(this); }
}

final class AST {
  XP root;
  Stack!XP opr, num;
  string xpr, token;
  int xpHead, xpTail;

  void joinXP(XP x) pure {
    x.RHS = num.pop;
    x.LHS = num.pop;
    num.push(x);
  }

  string nextToken() pure {
    while (xpHead < xpr.length && xpr[xpHead] == ' ')
      xpHead++; // Skip spc.
    xpTail = xpHead;
    if (xpHead < xpr.length) {
      token = xpr[xpTail .. xpTail + 1];
      switch (token) {
        case "(", ")", "+", "-", "*", "/": // Valid non-number.
          xpTail++;
          return token;
        default: // Should be number.
          if (token[0].isDigit) {
            while (xpTail < xpr.length && xpr[xpTail].isDigit())
              xpTail++;
            return xpr[xpHead .. xpTail];
          } // Else may be error.
      } // End switch.
    }
    if (xpTail < xpr.length)
      throw new Exception("Invalid Char <" ~ xpr[xpTail] ~ ">");
    return null;
  } // End nextToken.

  AST parse(in string s) {
    bool expectingOP;
    xpr = s;
    try {
      xpHead = xpTail = 0;
      num = opr = null;
      root = null;
      opr.push(new XP); // CBkt, prevent evaluate null OP precedence.
      while ((token = nextToken) !is null) {
        XP tokenXP = new XP(token, xpHead);
        if (expectingOP) { // Process OP-alike XP.
          switch (token) {
            case ")":
              while (opr.pop!false.type != Type.OBkt)
                joinXP(opr.pop);
              opr.pop;
              expectingOP = true;
              break;
            case "+", "-", "*", "/":
              while (tokenXP <= opr.pop!false)
                joinXP(opr.pop());
              opr.push(tokenXP);
              expectingOP = false;
              break;
            default:
              throw new Exception("Expecting Operator or ), not <"
                                  ~ token ~ ">");
          }
        } else { // Process Num-alike XP.
          switch (token) {
            case "+", "-", "*", "/", ")":
              throw new Exception("Expecting Number or (, not <"
                                  ~ token ~ ">");
            case "(":
              opr.push(tokenXP);
              expectingOP = false;
              break;
            default: // Number.
              num.push(tokenXP);
              expectingOP = true;
          }
        }
        xpHead = xpTail;
      } // End while.

      while (opr.length > 1) // Join pending Op.
        joinXP(opr.pop);
    } catch(Exception e) {
      writefln("%s\n%s\n%s^", e.msg, xpr, " ".replicate(xpHead));
      root = null;
      return this;
    }

    if (num.length != 1) { // Should be one XP left.
      "Parse Error...".writefln;
      root = null;
    } else {
      root = num.pop;
    }
    return this;
  } // End Parse.
}  // End class AST.

// To display AST fancy struct.
void ins(ref char[][] s, in string v, in int p, in int l)
pure nothrow {
  if (l + 1 > s.length)
    s.length++;
  while (s[l].length < p + v.length + 1)
    s[l] ~= " ";
  s[l][p .. p + v.length] = v[];
}

final class CalcVis : Visitor {
  int result, level;
  string resultStr;
  char[][] Tree;

  static void opCall(AST a) {
    if (a && a.root) {
      auto c = new CalcVis;
      a.root.accept(c);
      foreach (immutable i; 1 .. c.Tree.length) { // More fancy.
        bool flipflop = false;
        enum char mk = '.';
        foreach (immutable j; 0 .. c.Tree[i].length) {
          while (j >= c.Tree[i - 1].length)
            c.Tree[i - 1] ~= " ";
          immutable c1 = c.Tree[i][j];
          immutable c2 = c.Tree[i - 1][j];
          if (flipflop && (c1 == ' ') && c2 == ' ')
            c.Tree[i - 1][j] = mk;
          if (c1 != mk && c1 != ' ' &&
              (j == 0 || !isDigit(c.Tree[i][j - 1])))
            flipflop = !flipflop;
        }
      }
      foreach (const t; c.Tree)
        t.writefln;
      writefln("\n%s ==>\n%s = %s", a.xpr, c.resultStr, c.result);
    } else
      "Evalute invalid or null Expression.".writefln;
  }

  // Calc. the value, display AST struct and eval order.
  override void visit(XP xp) {
    ins(Tree, xp.str, xp.pos, level);
    level++;
    if (xp.type == Type.Num) {
      resultStr ~= xp.str;
      result = xp.str.to!int;
    } else {
      resultStr ~= "(";
      xp.LHS.accept(this);
      immutable int lhs = result;
      resultStr ~= opChar[xp.type];
      xp.RHS.accept(this);
      resultStr ~= ")";
      switch (xp.type) {
        case Type.Add: result = lhs + result; break;
        case Type.Sub: result = lhs - result; break;
        case Type.Mul: result = lhs * result; break;
        case Type.Div: result = lhs / result; break;
        default: throw new Exception("Invalid type");
      }
    }
    level--;
  }
}

void main(string[] args) {
  immutable exp0 = "1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5" ~
                   " - 22/(7 + 2*(3 - 1)) - 1)) + 1";
  immutable exp = (args.length > 1) ? args[1 .. $].join(" ") : exp0;
  new AST().parse(exp).CalcVis; // Should be 60.
}

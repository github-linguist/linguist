const thisFile = import.meta.url;

const waitOneTick = await Promise.resolve();

export function parse(
    source: string
  ): {
    test: string;
    consequent: string | null;
    alternate: string | null;
    hash: string | null;
  } {
    const PROTOCOL = "condition:";
  
    if (!source.startsWith(PROTOCOL, 0)) {
      throw new Error(`Expected 'condition:' at index 0 (${source})`);
    }
  
    let pos = PROTOCOL.length;
  
    skipWs();
  
    const test = eatRegExp(/[\w-]+/y);
    if (!test) {
      throw new Error(`Expected an identifier at index ${pos} (${source})`);
    }
  
    skipWs();
    expect("?");
    skipWs();
  
    let consequent = null;
    if (source[pos] === "(") {
      consequent = eatParenthesized().trim() || null;
      skipWs();
    } else if (source[pos] !== ":") {
      consequent = eatUntil(":").trimRight() || null;
    }
  
    expect(":");
    skipWs();
  
    let alternate = null;
    if (pos < source.length) {
      if (source[pos] === "(") {
        alternate = eatParenthesized().trim() || null;
        skipWs();
      } else if (source[pos] !== ":") {
        alternate = eatUntil("#").trimRight() || null;
      }
    }
  
    let hash = null;
    if (pos < source.length && source[pos] === "#") {
      pos++;
      hash = eatRegExp(/\w+/y);
      skipWs();
    }
  
    if (pos !== source.length) {
      throw new Error(`Unexpected '${source[pos]}' at index ${pos} (${source})`);
    }
  
    return { test, consequent, alternate, hash };
  
    function expect(ch: string) {
      if (source[pos] !== ch) {
        throw new Error(`Expected '${ch}' at index ${pos} (${source})`);
      }
      pos++;
    }
  
    function skipWs() {
      eatRegExp(/\s*/y);
    }
  
    function eatRegExp(re: RegExp) {
      re.lastIndex = pos;
      const match = re.exec(source);
      if (!match) return null;
  
      pos += match[0].length;
      return match[0];
    }
  
    function eatUntil(end: string) {
      const start = pos;
      pos = source.indexOf(end, start);
      if (pos === -1) pos = source.length;
      return source.slice(start, pos);
    }
  
    function eatParenthesized() {
      expect("(");
  
      let depth = 1;
      let contents = "";
  
      while (depth) {
        if (pos === source.length) {
          throw new Error(`Expected ')' at index ${pos} (${source})`);
        }
  
        const ch = source[pos];
  
        if (ch === "(") depth++;
        if (ch === ")") depth--;
  
        if (ch !== ")" || depth > 0) {
          contents += ch;
        }
  
        pos++;
      }
  
      return contents;
    }
  }
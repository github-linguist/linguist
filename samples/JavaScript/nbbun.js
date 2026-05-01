#!/usr/bin/env bun

/* 
Author: babashka on GitHub
Original source: https://github.com/babashka/nbb/blob/main/nbbun.js
License: Eclipse Public License 1.0 (https://github.com/babashka/nbb/blob/main/LICENSE)
*/

import { loadString, loadFile, addClassPath } from './index.mjs';
import { existsSync } from "node:fs";

const prn = await loadString('prn');
const nonNil = await loadString('some?');

var args = process.argv.slice(2);

if (args[0] === '--classpath') {
  addClassPath(args[[1]]);
  args = args.slice(2);
}

if (args[0] === '-e') {
  let res = await(loadString(args[1]));
  if (nonNil(res)) {
    prn(res);
  }
  args = args.slice(2);
}

if (args[0] === '--help') {
  console.log(`Usage: nbbun <opts> <file>

Options:

--classpath: set classpath.
-e <expr>: run expressions and print result.
--help: print this help.
`);
}

if (args[0] && existsSync(args[0])) {
  await(loadFile(args[0]));
  args = args.slice(2);
}
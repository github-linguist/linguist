module examples/systems/file_system

/*
 * Model of a generic file system.
 */

abstract sig Object {}

sig Name {}

sig File extends Object {} { some d: Dir | this in d.entries.contents }

sig Dir extends Object {
  entries: set DirEntry,
  parent: lone Dir
} {
  parent = this.~@contents.~@entries
  all e1, e2 : entries | e1.name = e2.name => e1 = e2
  this !in this.^@parent
  this != Root => Root in this.^@parent
}

one sig Root extends Dir {} { no parent }

lone sig Cur extends Dir {}

sig DirEntry {
  name: Name,
  contents: Object
} {
  one this.~entries
}


/**
 * all directories besides root have one parent
 */
pred OneParent_buggyVersion {
    all d: Dir - Root | one d.parent
}

/**
 * all directories besides root have one parent
 */
pred OneParent_correctVersion {
    all d: Dir - Root | (one d.parent && one contents.d)
}

/**
 * Only files may be linked (that is, have more than one entry)
 * That is, all directories are the contents of at most one directory entry
 */
pred NoDirAliases {
    all o: Dir | lone o.~contents
}

check { OneParent_buggyVersion => NoDirAliases } for 5 expect 1

check { OneParent_correctVersion => NoDirAliases } for 5 expect 0

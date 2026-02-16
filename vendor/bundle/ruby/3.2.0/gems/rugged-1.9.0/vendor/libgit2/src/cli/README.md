# cli

A git-compatible command-line interface that uses libgit2.

## Adding commands

1. Individual commands have a `main`-like top-level entrypoint.  For example:

   ```c
   int cmd_help(int argc, char **argv)
   ```

   Although this is the same signature as `main`, commands are not built as
   individual standalone executables, they'll be linked into the main cli.
   (Though there may be an option for command executables to be built as
   standalone executables in the future.)

2. Commands are prototyped in `cmd.h` and added to `main.c`'s list of
   commands (`cli_cmds[]`).  Commands should be specified with their name,
   entrypoint and a brief description that can be printed in `git help`.
   This is done because commands are linked into the main cli.

3. Commands should accept a `--help` option that displays their help
   information.  This will be shown when a user runs `<command> --help` and
   when a user runs `help <command>`.


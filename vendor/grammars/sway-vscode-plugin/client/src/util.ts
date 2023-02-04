import { inspect } from 'util';
import * as vscode from 'vscode';

export const addFilePrefix = (path: string) => `file://${path}`;

export const log = new (class {
  private enabled = true;
  private readonly output =
    vscode.window.createOutputChannel('Sway LSP Client');

  setEnabled(yes: boolean): void {
    log.enabled = yes;
  }

  // Hint: the type [T, ...T[]] means a non-empty array
  debug(...msg: [unknown, ...unknown[]]): void {
    if (!log.enabled) return;
    log.write('DEBUG', ...msg);
  }

  info(...msg: [unknown, ...unknown[]]): void {
    log.write('INFO', ...msg);
  }

  warn(...msg: [unknown, ...unknown[]]): void {
    log.write('WARN', ...msg);
  }

  error(...msg: [unknown, ...unknown[]]): void {
    log.write('ERROR', ...msg);
    log.output.show(true);
  }

  private write(label: string, ...messageParts: unknown[]): void {
    const message = messageParts.map(log.stringify).join(' ');
    const dateTime = new Date().toLocaleString();
    log.output.appendLine(`${label} [${dateTime}]: ${message}`);
  }

  private stringify(val: unknown): string {
    if (typeof val === 'string') return val;
    return inspect(val, {
      colors: false,
      depth: 6, // heuristic
    });
  }
})();

// Utilities for interacting with VSCode terminals.
export namespace Terminal {
  type Names = 'sway' | 'fuel-core';
  class NamedTerminal {
    static type: Names;
    static execute(cmd: string): void {
      const terminal = this.get();
      terminal.sendText(cmd);
      terminal.show(true);
    }

    private static get(): vscode.Terminal {
      const existing = vscode.window.terminals.find(t => t.name === this.type);
      return existing ?? vscode.window.createTerminal(this.type);
    }
  }
  // Used to execute any commands that are not long-running.
  export class Sway extends NamedTerminal {
    static type: Names = 'sway';
  }
  // Only used for starting fuel-core, which is a long-running process.
  export class FuelCore extends NamedTerminal {
    static type: Names = 'fuel-core';
  }
}

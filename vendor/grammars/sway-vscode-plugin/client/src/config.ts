import * as vscode from 'vscode';
import { log } from './util';

export class Config {
  readonly extensionId = 'fuellabs.sway-vscode-plugin';
  readonly rootSection = 'sway-lsp';
  private readonly requiresReloadOpts = ['debug'].map(
    opt => `${this.rootSection}.${opt}`
  );

  readonly package: {
    version: string;
  } = vscode.extensions.getExtension(this.extensionId)!.packageJSON;

  readonly globalStorageUri: vscode.Uri;

  constructor(ctx: vscode.ExtensionContext) {
    this.globalStorageUri = ctx.globalStorageUri;
    vscode.workspace.onDidChangeConfiguration(
      this.onDidChangeConfiguration,
      this,
      ctx.subscriptions
    );
    this.refreshLogging();
  }

  private refreshLogging() {
    log.setEnabled(this.traceExtension);
    log.info('Starting the Sway Language Client and Server');
    log.info('Extension version:', this.package.version);

    const cfg = Object.entries(this.cfg).filter(
      ([_, val]) => !(val instanceof Function)
    );
    log.info('Using configuration', Object.fromEntries(cfg));
  }

  private async onDidChangeConfiguration(
    event: vscode.ConfigurationChangeEvent
  ) {
    this.refreshLogging();

    const requiresReloadOpt = this.requiresReloadOpts.find(opt =>
      event.affectsConfiguration(opt)
    );

    if (!requiresReloadOpt) return;

    const userResponse = await vscode.window.showInformationMessage(
      `Changing "${requiresReloadOpt}" requires a reload`,
      'Reload now'
    );

    if (userResponse === 'Reload now') {
      await vscode.commands.executeCommand('workbench.action.reloadWindow');
    }
  }

  // We don't do runtime config validation here for simplicity. More on stackoverflow:
  // https://stackoverflow.com/questions/60135780/what-is-the-best-way-to-type-check-the-configuration-for-vscode-extension

  private get cfg(): vscode.WorkspaceConfiguration {
    return vscode.workspace.getConfiguration(this.rootSection);
  }

  /**
   * Beware that postfix `!` operator erases both `null` and `undefined`.
   * This is why the following doesn't work as expected:
   *
   * ```ts
   * const nullableNum = vscode
   *  .workspace
   *  .getConfiguration
   *  .getConfiguration("sway-lsp")
   *  .get<number | null>(path)!;
   *
   * // What happens is that type of `nullableNum` is `number` but not `null | number`:
   * const fullFledgedNum: number = nullableNum;
   * ```
   * So this getter handles this quirk by not requiring the caller to use postfix `!`
   */
  private get<T>(path: string): T {
    return this.cfg.get<T>(path)!;
  }

  get traceExtension() {
    return this.get<boolean>('trace.extension');
  }
}

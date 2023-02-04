import { commands, ExtensionContext, ExtensionMode, workspace } from 'vscode';
import * as lc from 'vscode-languageclient/node';
import { createClient, getClient } from './client';
import { Config } from './config';
import { CommandPalettes } from './palettes';
import updateFuelCoreStatus from './status_bar/fuelCoreStatus';
import { log } from './util';

export function activate(context: ExtensionContext) {
  const config = new Config(context);

  // Register all command palettes
  const commandPalettes = new CommandPalettes(config).get();
  context.subscriptions.push(
    ...commandPalettes.map(({ command, callback }) =>
      commands.registerCommand(command, callback)
    )
  );

  // Start a recurring task to keep fuel-core status updated
  setInterval(updateFuelCoreStatus, 1000);

  const client = createClient(
    getClientOptions(),
    getServerOptions(context, config)
  );

  // Start the client. This will also launch the server
  client.start();

  log.info('Client has Connected to the Sway Language Server Successfully!');
}

export function deactivate(): Thenable<void> | undefined {
  const client = getClient();
  if (!client) {
    return undefined;
  }
  return client.stop();
}

function getServerOptions(
  context: ExtensionContext,
  config: Config
): lc.ServerOptions {
  const serverExecutable: lc.Executable = {
    command: 'forc',
    args: ['lsp'],
    options: {
      shell: true,
    },
  };

  const devServerOptions: lc.ServerOptions = {
    run: serverExecutable,
    debug: serverExecutable,
    transport: lc.TransportKind.stdio,
  };

  switch (context.extensionMode) {
    case ExtensionMode.Development:
    case ExtensionMode.Test:
      return devServerOptions;

    default:
      // TODO: for production we need to be able to install the Language Server
      return devServerOptions;
  }
}

function getClientOptions(): lc.LanguageClientOptions {
  // Options to control the language client
  const clientOptions: lc.LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [
      { scheme: 'file', language: 'sway' },
      { scheme: 'untitled', language: 'sway' },
    ],
    synchronize: {
      // Notify the server about file changes to *.sw files contained in the workspace
      fileEvents: [
        workspace.createFileSystemWatcher('**/.sw'),
        workspace.createFileSystemWatcher('**/*.sw'),
      ],
    },
    initializationOptions: workspace.getConfiguration('sway-lsp'),
  };

  return clientOptions;
}

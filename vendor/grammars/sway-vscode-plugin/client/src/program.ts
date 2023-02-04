import * as fs from 'fs';
import * as path from 'path';
// import * as fuels from 'typechain-target-fuels';
import {
  Event,
  EventEmitter,
  ProviderResult,
  TreeDataProvider,
  TreeItem,
  TreeItemCollapsibleState,
  window,
} from 'vscode';

const ABI_FILE_SUFFIX = '-abi.json';

export class ProgramProvider implements TreeDataProvider<Function | Program> {
  private _onDidChangeTreeData: EventEmitter<Program | undefined | void> =
    new EventEmitter<Program | undefined | void>();
  readonly onDidChangeTreeData: Event<Program | undefined | void> =
    this._onDidChangeTreeData.event;

  constructor(
    private workspaceRoot: string | undefined,
    readonly type: ProgramType
  ) {}

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  getTreeItem(element: TreeItem): TreeItem {
    return element;
  }

  getChildren(contract?: Function | Program): ProviderResult<Program[]> {
    if (!this.workspaceRoot) {
      window.showInformationMessage('No contract in empty workspace');
      return Promise.resolve([]);
    }

    return contract
      ? Promise.resolve(contract['children'])
      : this.getPrograms();
  }

  /**
   * Reads contracts from the ABIs.
   * @returns array of contracts
   */
  private async getPrograms(): Promise<Program[]> {
    const allFiles = getAllFiles(this.workspaceRoot, []);
    const swayFilePaths = allFiles.filter(file => file.endsWith('.sw'));
    const forcTomlFilePaths = allFiles.filter(file =>
      file.endsWith('Forc.toml')
    );
    const abiFilePaths = allFiles.filter(file =>
      file.endsWith(ABI_FILE_SUFFIX)
    );
    const programs = abiFilePaths.map(filepath => {
      const contractName = path
        .parse(filepath)
        .base.replace(ABI_FILE_SUFFIX, '');

      // This is assuming that there is only one sway contract in the same directory as a Forc.toml
      const forcFilePath = forcTomlFilePaths.find(forcFilePath =>
        fs.readFileSync(forcFilePath).toString().includes(contractName)
      );

      // Find out the program type of the sway file
      const swayFilePath = swayFilePaths.find(swayFilePath => {
        return swayFilePath.startsWith(path.parse(forcFilePath).dir);
      });
      const swayFileMatchesType = fs
        .readFileSync(swayFilePath)
        .toString()
        .startsWith(this.type);
      if (!swayFileMatchesType) return undefined;

      // Attach the source file path to each function node in addition to the contract node
      const buffer = fs.readFileSync(filepath);
      const abi: Object[] = JSON.parse(buffer.toString());
      const functions = abi
        .filter(obj => obj['type'] === 'function')
        .map(func => new Function(func['name'], swayFilePath));

      return new Program(contractName, swayFilePath, functions, this.type);
    });

    // Filter out programs that do not match the given type
    return programs.filter(program => !!program);
  }
}

/**
 * Recursively finds all ABI files in the workspace
 * @param dirPath path to current directory
 * @param arrayOfFiles array of files to search
 * @returns list of all ABI file paths
 */
const getAllFiles = (dirPath: string, arrayOfFiles: string[]): string[] => {
  let files = fs.readdirSync(dirPath);

  arrayOfFiles = arrayOfFiles || [];

  files.forEach(function (file) {
    if (fs.statSync(dirPath + '/' + file).isDirectory()) {
      arrayOfFiles = getAllFiles(dirPath + '/' + file, arrayOfFiles);
    } else {
      arrayOfFiles.push(path.join(dirPath, '/', file));
    }
  });

  return arrayOfFiles;
};

export type ProgramType = 'contract' | 'script' | 'predicate';

export class Program extends TreeItem {
  constructor(
    public readonly name: string,
    public readonly sourceFilePath: string,
    public readonly children: Function[],
    public readonly type?: ProgramType
  ) {
    super(
      name,
      children
        ? TreeItemCollapsibleState.Expanded
        : TreeItemCollapsibleState.None
    );

    this.label = name;
    this.tooltip = name;
  }

  contextValue = this.type ?? 'program';
}

export class Function extends TreeItem {
  constructor(
    public readonly label: string,
    public readonly sourceFilePath: string
  ) {
    super(label, TreeItemCollapsibleState.None);

    this.tooltip = this.label;
  }

  contextValue = 'function';
}

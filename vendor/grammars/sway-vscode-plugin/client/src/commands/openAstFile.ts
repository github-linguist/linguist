import { commands, Uri } from 'vscode';
import { AstKind, showAst } from '../interface/showAst';
import { addFilePrefix, log } from '../util';

export default async function openAstFile(filePath: string, astKind: AstKind) {
  try {
    const astDocument = await showAst(addFilePrefix(filePath), astKind);
    if (astDocument) {
      const openPath = Uri.parse(astDocument.uri);
      await commands.executeCommand('vscode.openFolder', openPath, {
        forceNewWindow: true,
      });
      log.info(`Successfully opened ${astKind} AST file ${filePath}`);
    } else {
      log.error(`No ${astKind} AST file found for ${filePath}`);
    }
  } catch (error) {
    log.error(
      `Failed to open ${astKind} AST file found for ${filePath}`,
      error
    );
  }
}

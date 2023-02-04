import { Range } from 'vscode';
import {
  RequestType,
  TextDocumentIdentifier,
} from 'vscode-languageclient/node';
import { getClient } from '../client';
import { ProgramType } from '../program';
import { addFilePrefix } from '../util';

interface GetRunnablesParams {
  textDocument: TextDocumentIdentifier;
}

export type Runnable = [Range, ProgramType];

const request = new RequestType<GetRunnablesParams, Runnable[], void>(
  'sway/runnables'
);

export const getRunnables = async (filePath: string): Promise<Runnable[]> => {
  const client = getClient();
  const params: GetRunnablesParams = {
    textDocument: {
      uri: addFilePrefix(filePath),
    },
  };
  const response = await client.sendRequest(request, params);
  return response ?? [];
};

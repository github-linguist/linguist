import { window } from 'vscode';
import { Terminal } from '../util';

export default function startFuelCore() {
  Terminal.FuelCore.execute(`fuel-core run --db-type in-memory`);
  window.showInformationMessage(`Started fuel-core`);
}

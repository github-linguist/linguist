import { exec } from 'child_process';
import { window } from 'vscode';
import updateFuelCoreStatus from '../status_bar/fuelCoreStatus';

export default function stopFuelCore() {
  // Using exec intead of the Terminal utlity here because we don't need the user to see the
  // command output.
  exec(`pkill -15 fuel-core`, () => {
    window.showInformationMessage(`Stopped fuel-core`);
    updateFuelCoreStatus();
  });
}

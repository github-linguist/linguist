import { exec } from 'child_process';
import { StatusBarAlignment, StatusBarItem, ThemeColor, window } from 'vscode';

let fuelCoreStatus: StatusBarItem;
let isFuelCoreRunning: boolean;

export const getFuelCoreStatus = () => {
  if (!fuelCoreStatus) {
    fuelCoreStatus = window.createStatusBarItem(StatusBarAlignment.Left, 100);
  }
  return fuelCoreStatus;
};

export default function updateFuelCoreStatus() {
  const initializedItem = getFuelCoreStatus();
  exec(`ps aux | grep -i fuel-cor`, (_error, stdout, _stderr) => {
    isFuelCoreRunning = stdout.includes('fuel-core');
  });

  if (isFuelCoreRunning) {
    initializedItem.text = '$(symbol-event) running';
    initializedItem.command = 'sway.stopFuelCore';
    initializedItem.tooltip = 'Stop the locally running fuel-core server';
    // Using any string other than the approved theme colors will reset the background to default.
    initializedItem.backgroundColor = new ThemeColor('reset');
    initializedItem.show();
  } else {
    initializedItem.text = '$(symbol-event) stopped';
    initializedItem.command = 'sway.startFuelCore';
    initializedItem.tooltip = 'Start fuel-core server at 127.0.0.1:4000';
    initializedItem.backgroundColor = new ThemeColor(
      'statusBarItem.warningBackground'
    );
    initializedItem.show();
  }
}

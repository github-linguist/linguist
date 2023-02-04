import { Terminal } from '../util';

export default function forcBuild(forcDir: string) {
  Terminal.Sway.execute(`cd ${forcDir} && forc build`);
}

import { Terminal } from '../util';

export default function forcTest(forcDir: string, _testName?: string) {
  // TODO: add support for running specific tests when
  // https://github.com/FuelLabs/sway/issues/3268 is resolved
  Terminal.Sway.execute(`cd ${forcDir} && forc test`);
}

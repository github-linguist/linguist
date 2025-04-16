import cp = require("child_process");

export = function run(command: string): Promise<{ stdout: string; stderr: string }> {
  const s = (b) => String(b).trim();

  return new Promise((resolve, reject) => {
    cp.exec(command, (error, stdout, stderr) => {
      if (error) return reject(error);
      resolve({ stdout: s(stdout), stderr: s(stderr) });
    });
  });
}

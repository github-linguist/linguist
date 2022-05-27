import { exec } from "child_process";

exports function run(command: string): Promise<{ stdout: string; stderr: string }> {
  const s = (b) => String(b).trim();

  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) return reject(error);
      resolve({ stdout: s(stdout), stderr: s(stderr) });
    });
  });
}

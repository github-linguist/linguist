int main(){
   // Process.run was added in Pike 7.8 as a wrapper to simplify the use of Process.create_process()
   mapping response = Process.run("ls -l");
   // response is now a map containing 3 fields
   // stderr, stdout, and exitcode. We want stdout.
   write(response["stdout"] + "\n");

   // with older versions of pike it's a bit more complicated:
   Stdio.File stdout = Stdio.File();
   Process.create_process(({"ls", "-l"}), ([ "stdout" : stdout->pipe() ]) );
   write(stdout->read() + "\n");
}

export const verilog_state = state("verilog", ["sv", "v"]);
export const verilog_noverify = state("verilog-noverify", ["sv"]);
export const calyx_state = state("calyx", ["futil"]);

export let calyx_setup = calyx_setup;
fn calyx_setup(e) {
   e.config_var("calyx-base", "calyx.base");
   e.config_var_or("calyx-exe", "calyx.exe", "$calyx-base/target/debug/calyx");
   e.config_var_or("calyx-lib-path", "calyx.lib_path", "$calyx-base");
   e.config_var_or("args", "calyx.args", "");

   e.rule("calyx", "$calyx-exe -l $calyx-lib-path -b $backend $args $in > $out");
   e.rule("calyx-pass", "$calyx-exe -l $calyx-lib-path -p $pass $args $in > $out");

   e.config_var_or("cider-calyx-passes", "cider.calyx-passes", "-p none");
   e.rule(
       "calyx-cider",
       "$calyx-exe -l $calyx-lib-path $cider-calyx-passes $args $in > $out",
   );
}

op(
  "calyx-to-verilog",
  [calyx_setup],
  calyx_state,
  verilog_state,
  |e, input, output| {
    e.build_cmd([output], "calyx", [input], []) ;
    e.arg("backend", "verilog");
  }
);


op(
    "calyx-noverify",
    [calyx_setup],
    calyx_state,
    verilog_noverify,
    |e, input, output| {
        // Icarus requires a special --disable-verify version of Calyx code.
        e.build_cmd([output], "calyx", [input], []);
        e.arg("backend", "verilog");
        e.arg("args", "--disable-verify $args");
    },
);

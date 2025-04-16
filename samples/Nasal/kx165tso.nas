###############################################################################
##
## Nasal for dual control of a KX165 NavComm radio over the multiplayer
## network.
##
##  Copyright (C) 2007 - 2011  Anders Gidenstam  (anders(at)gidenstam.org)
##  This file is licensed under the GPL license version 2 or later.
##
###############################################################################

# Note:
#  This module MUST be loaded as KX165.
#

# Slave button presses.
var swap_btn    = "frq-swap-btn";
var freq_decS   = "freq-decS-clicked";
var freq_incS   = "freq-incS-clicked";
var freq_decL   = "freq-decL-clicked";
var freq_incL   = "freq-incL-clicked";

# Settings
var freq_selected = "frequencies/selected-mhz";
var freq_standby  = "frequencies/standby-mhz";

var comm_base = ["instrumentation/comm[0]",
                 "instrumentation/comm[1]"];
var nav_base = ["instrumentation/nav[0]",
                "instrumentation/nav[1]"];

###########################################################################
var master_kx165tso = {
  new : func(n) {
    var obj = {};
    obj.parents = [master_kx165tso];
    obj.nav_base  = props.globals.getNode("instrumentation/nav[" ~ n ~ "]");
    obj.comm_base = props.globals.getNode("instrumentation/comm[" ~ n ~ "]");
    return obj;
  },
  swap_nav : func() {
    var tmp = me.nav_base.getNode(freq_selected).getValue();
    me.nav_base.getNode(freq_selected).setValue
      (me.nav_base.getNode(freq_standby).getValue());
    me.nav_base.getNode(freq_standby).setValue(tmp);
  },
  swap_comm : func() {
    var tmp = me.comm_base.getNode(freq_selected).getValue();
    me.comm_base.getNode(freq_selected).setValue
      (me.comm_base.getNode(freq_standby).getValue());
    me.comm_base.getNode(freq_standby).setValue(tmp);
  },
  adjust_nav_frequency : func(d) {
    adjust_radio_frequency(
      me.nav_base.getNode(freq_standby),
      d,
      108,
      117.95);
  },
  adjust_comm_frequency : func(d) {
    adjust_radio_frequency(
      me.comm_base.getNode(freq_standby),
      d,
      118,
      135.975);
  }
};

###########################################################################
var slave_kx165tso = {
  new : func(n, airoot) {
    var obj = {};
    obj.parents = [slave_kx165tso];
    obj.root = airoot;
    obj.nav_base  = props.globals.getNode("instrumentation/nav[" ~ n ~ "]");
    obj.comm_base = props.globals.getNode("instrumentation/comm[" ~ n ~ "]");
    return obj;
  },
  swap_nav : func() {
    var p = me.nav_base.getNode(swap_btn);
#    print("KX165tso[?].NAVSWAP");
    if (!p.getValue()) {
      p.setValue(1);
      settimer(func { p.setValue(0); },
               1.0);
    }
  },
  swap_comm : func() {
    var p = me.comm_base.getNode(swap_btn);
#    print("KX165tso[?].COMMSWAP");
    if (!p.getValue()) {
      p.setValue(1);
      settimer(func { p.setValue(0); },
               1.0);
    }
  },
  adjust_nav_frequency : func(d) {
    var p = 0;
    if (abs(d) < 0.99) {
      p = (d < 0) ? me.nav_base.getNode(freq_decS)
                  : me.nav_base.getNode(freq_incS);
    } else {
      p = (d < 0) ? me.nav_base.getNode(freq_decL)
                  : me.nav_base.getNode(freq_incL);
    }
    if (!p.getValue()) {
      p.setValue(1);
      settimer(func { p.setValue(0); },
               1.0);
    }
  },
  adjust_comm_frequency : func(d) {
    var p = 0;
    if (abs(d) < 0.99) {
      p = (d < 0) ? me.comm_base.getNode(freq_decS)
                  : me.comm_base.getNode(freq_incS);
    } else {
      p = (d < 0) ? me.comm_base.getNode(freq_decL)
                  : me.comm_base.getNode(freq_incL);
    }
    if (!p.getValue()) {
      p.setValue(1);
      settimer(func { p.setValue(0); },
               1.0);
    }
  }
};

###########################################################################
#  The KX-165 pick animations default to master.
#  NOTE: Use make_master() and make_slave_to().
#        Do NOT change kx165tso directly.
var kx165tso = [master_kx165tso.new(0), master_kx165tso.new(1)];


###########################################################################
# API for pick animations and dual control setup.
###########################################################################

###########################################################################
# n - NavComm#
var make_master = func(n) {
  kx165tso[n] = master_kx165tso.new(n);
}

###########################################################################
# n - NavComm#
var make_slave_to = func(n, airoot) {
  kx165tso[n] = slave_kx165tso.new(n, airoot);
}

###########################################################################
# n - NavComm#
var swap_nav = func(n) {
  kx165tso[n].swap_nav();
}

###########################################################################
# n - NavComm#
var swap_comm = func(n, b) {
  kx165tso[n].comm_base.getNode(swap_btn, 1).setValue(b);
  if (b) kx165tso[n].swap_comm();
}

###########################################################################
# n - NavComm#
# d - adjustment
var adjust_nav_frequency = func(n, d) {
  kx165tso[n].adjust_nav_frequency(d);
}

###########################################################################
# n - NavComm#
# d - adjustment
var adjust_comm_frequency = func(n, d) {
  kx165tso[n].adjust_comm_frequency(d);
}

###########################################################################
# Create aliases to drive a radio 3d model in an AI/MP model. 
# n - NavComm#
var animate_aimodel = func(n, airoot) {
  # Comm
  var base = comm_base[n];
  var p = "systems/electrical/outputs/comm["~ n ~"]";
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = "instrumentation/comm["~ n ~"]/serviceable";
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ freq_selected;
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ freq_standby;
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ swap_btn;
  props.globals.getNode(p, 1).alias(airoot.getNode(p));
  # Nav
  base = nav_base[n];
  p = "systems/electrical/outputs/nav["~ n ~"]";
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = "instrumentation/nav["~ n ~"]/serviceable";
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ freq_selected;
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ freq_standby;
  airoot.getNode(p, 1).alias(props.globals.getNode(p));
  p = base ~ "/" ~ swap_btn;
  props.globals.getNode(p, 1).alias(airoot.getNode(p));
}

###########################################################################
# Create a TDMEncoder node array for sending the current radio state to
# slaves.  
# n - NavComm#
var master_send_state = func(n) {
  var cb = props.globals.getNode(comm_base[n]);
  var nb = props.globals.getNode(nav_base[n]);
  return
    [
     cb.getNode(freq_selected),
     cb.getNode(freq_standby),
     nb.getNode(freq_selected),
     nb.getNode(freq_standby)
    ];
}

###########################################################################
# Create a SwitchDecoder action array for processing button presses
# from a slave.  
# n - Comm#
var master_receive_slave_buttons = func(n) {
  return
    [
     # Comm
     func (b) {
         if (b) {
             swap_comm(n, 1);
             settimer(func { swap_comm(n, 0); }, 1.0)
         }
     },
     func (b) {
         if (b) { adjust_comm_frequency(n, -0.025); }
     },
     func (b) {
         if (b) { adjust_comm_frequency(n, 0.025); }
     },
     func (b) {
         if (b) { adjust_comm_frequency(n, -1.0); }
     },
     func (b) {
         if (b) { adjust_comm_frequency(n, 1.0); }
     },
     # Nav
     func (b) {
         if (b) { swap_nav(n); }
     },
     func (b) {
         if (b) { adjust_nav_frequency(n, -0.05); }
     },
     func (b) {
         if (b) { adjust_nav_frequency(n, 0.05); }
     },
     func (b) {
         if (b) { adjust_nav_frequency(n, -1.0); }
     },
     func (b) {
         if (b) { adjust_nav_frequency(n, 1.0); }
     }
    ];
}

###########################################################################
# Create a TDMDecoder action array for processing the radio state
# from the master.
# n - NavComm#
var slave_receive_master_state = func(n) {
  var cb = props.globals.getNode(comm_base[n]);
  var nb = props.globals.getNode(nav_base[n]);
  return
    [
     func (v) {
         cb.getNode(freq_selected).setValue(v);
     },
     func (v) {
         cb.getNode(freq_standby).setValue(v);
     },
     func (v) {
         nb.getNode(freq_selected).setValue(v);
     },
     func (v) {
         nb.getNode(freq_standby).setValue(v);
     }
    ];
}

###########################################################################
# Create a SwitchEncoder node array for sending button presses
# to the master
# n - NavComm#
var slave_send_buttons = func(n) {
  var cb = props.globals.getNode(comm_base[n]);
  var nb = props.globals.getNode(nav_base[n]);
  return
    [
     # Comm
     cb.getNode(swap_btn, 1),
     cb.getNode(freq_decS, 1),
     cb.getNode(freq_incS, 1),
     cb.getNode(freq_decL, 1),
     cb.getNode(freq_incL, 1),
     # Nav
     nb.getNode(swap_btn, 1),
     nb.getNode(freq_decS, 1),
     nb.getNode(freq_incS, 1),
     nb.getNode(freq_decL, 1),
     nb.getNode(freq_incL, 1)     
    ];
}



###########################################################################
# Generic frequency stepper.
#  f   - frequency property
#  d   - change
#  min - min frequency
#  max - max frequency
var adjust_radio_frequency = func(f, d, min, max) {
  var old = f.getValue();
  var new = old + d;
  if (new < min - 0.005) { new = int(max) + (new - int(new)); }
  if (new > max + 0.005) {
      new = int(min) + (new - int(new));
      if (int(new + 0.005) > min) new -= 1;
  }
#  print("Old: " ~ old ~ "  Intermediate: " ~ (old + d) ~ "  New: " ~ new);
  f.setValue(new);
}

param num_beams;                  # number of beams

param num_rows >= 1, integer;     # number of rows
param num_cols >= 1, integer;     # number of columns 

set BEAMS   := 1 .. num_beams;    # set of beams

set ROWS    := 1 .. num_rows;	  # set of rows
set COLUMNS := 1 .. num_cols;	  # set of columns

# values for entries of each beam
param beam_values {BEAMS, ROWS, COLUMNS} >= 0; 

# values of tumor
param tumor_values {ROWS, COLUMNS} >= 0; 

# values of critical area
param critical_values {ROWS, COLUMNS} >= 0; 

# critical maximum dosage requirement
param critical_max;

# tumor minimum dosage requirement
param tumor_min;

# dosage scalar of each beam
var X {i in BEAMS} >= 0;


# define the tumor area which includes the locations where tumor exists
set tumor_area := {k in ROWS, h in COLUMNS: tumor_values[k,h] > 0};

# define critical area 
set critical_area := {k in ROWS, h in COLUMNS: critical_values[k,h] > 0};

var S {(k,h) in tumor_area} >= 0;
var T {(k,h) in critical_area} >= 0;

# maximize total dosage in tumor area
maximize total_tumor_dosage: sum {i in BEAMS} sum {(k,h) in tumor_area} X[i] * beam_values[i,k,h];

# minimize total dosage in critical area
minimize total_critical_dosage: sum {i in BEAMS} sum {(k,h) in critical_area} X[i] * beam_values[i,k,h];

# minimize total tumor slack
minimize total_tumor_slack: sum {(k,h) in tumor_area} S[k,h];

# minimize total critical area slack
minimize total_critical_slack: sum {(k,h) in critical_area} T[k,h];

# total dosage at each tumor location [k,h] should be >= min tumor dosage with slack variable
subject to tumor_limit {(k,h) in tumor_area} : sum {i in BEAMS} X[i] * beam_values[i,k,h] == tumor_min - S[k,h];

# total dosage at each critical location [k,h] should be = max critical dosage with slack variable
subject to critical_limit {(k,h) in critical_area} : sum {i in BEAMS} X[i] * beam_values[i,k,h] == critical_max + T[k,h];




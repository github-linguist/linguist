%macro check_dataset(dset =, obs_lim = max, eldest_age = 89) ;
  %local i ;
  %local inset_name ;
  %let inset_name = &dset ;

  %if %lowcase(&obs_lim) = max %then %do ;
    %** Nothing ;
  %end ;
  %else %do ;
    proc surveyselect
      data      = &inset_name
      out       = __sub_dset
      method    = srs
      sampsize  = &obs_lim SELECTALL
      seed      = 1234567
      noprint
    ;
    run;
    %let dset = __sub_dset ;
  %end ;

  %macro check_varname(regx, msg) ;
    create table possible_bad_vars as
    select name, label
    from these_vars
    where prxmatch(compress("/(&regx)/i"), name)
    ;

    %if &sqlobs > 0 %then %do ;
      insert into phi_warnings(dset, variable, label, warning)
      select "&inset_name" as dset, name, label, "&msg"
      from possible_bad_vars
      ;
    %end ;

  %mend check_varname ;

  %macro check_vars_for_mrn(length_limit = 6, obs_lim = max) ;
    %local char ;
    %let char = 2 ;
    proc sql noprint ;
      select name
      into :mrn_array separated by ' '
      from these_vars
      where type = &char and length ge &length_limit
      ;
    quit ;
    %if &sqlobs > 0 %then %do ;
      %put Checking these vars for possible MRN contents: &mrn_array ;
      data __gnu ;
        retain
          mrn_regex_handle
          badcount
        ;
        set &inset_name (obs = &obs_lim keep = &mrn_array) ;
        if _n_ = 1 then do ;
          mrn_regex_handle = prxparse("/&mrn_regex/") ;
          badcount = 0 ;
        end ;
        array p &mrn_array ;
        do i = 1 to dim(p) ;
          if prxmatch(mrn_regex_handle, p{i}) then do ;
            badvar = vname(p{i}) ;
            badvalue = p{i} ;
            badcount = _n_ ;
            output ;
          end ;
          keep badvar badvalue badcount ;
        end ;
      run ;
      proc sql noprint ;
        select compress(put(max(badcount), best.))
        into :badcount
        from __gnu
        ;
        insert into phi_warnings(dset, variable, warning)
        select distinct "&inset_name", badvar, "Could this var hold MRN values?  Contents of %trim(&badcount) records match the pattern given for MRN values.  MRNs should never move across sites."
        from __gnu ;
        drop table __gnu ;
      quit ;
    %end ;
  %mend check_vars_for_mrn ;

  %macro check_vars_for_oldsters(eldest_age = 89, obs_lim = max) ;
    %local dtfmts ;
    %let dtfmts = 'B8601DA','B8601DN','B8601DT','B8601DZ','B8601LZ','B8601TM','B8601TZ','DATE','DATEAMPM','DATETIME','DAY','DDMMYY',
                  'DDMMYYB','DDMMYYC','DDMMYYD','DDMMYYN','DDMMYYP','DDMMYYS','DOWNAME','DTDATE','DTMONYY','DTWKDATX','DTYEAR',
                  'DTYYQC','E8601DA','E8601DN','E8601DT','E8601DZ','E8601LZ','E8601TM','E8601TZ','HHMM','HOUR','JULDAY','JULIAN',
                  'MMDDYY','MMDDYYB','MMDDYYC','MMDDYYD','MMDDYYN','MMDDYYP','MMDDYYS','MMSS','MMYY','MMYY','MONNAME','MONTH','MONYY',
                  'PDJULG','PDJULI','QTR','QTRR','WEEKDATE','WEEKDATX','WEEKDAY','WEEKU','WEEKV','WEEKW','WORDDATE','WORDDATX',
                  'YEAR','YYMM','YYMMC','YYMMD','YYMMN','YYMMP','YYMMS','YYMMDD','YYMMDDB','YYMMDDC','YYMMDDD','YYMMDDN','YYMMDDP',
                  'YYMMDDS','YYMON','YYQ','YYQC','YYQD','YYQN','YYQP','YYQS','YYQR','YYQRC','YYQRD','YYQRN','YYQRP','YYQRS' ;

    %local num ;
    %let num = 1 ;

    proc sql noprint ;
      select name
      into :dat_array separated by ' '
      from these_vars
      where type = &num and (format in (&dtfmts) or lowcase(name) like '%date%')
      ;
      /* added by cb to shorten the process of looking at all dates */
      %if &sqlobs > 0 %then %do ;
        %put Checking these vars for possible DOB contents: &dat_array ;
        select 'min(' || trim(name) || ') as ' || name into :var_list separated by ','
        from these_vars
        where type = &num and (format in (&dtfmts) or lowcase(name) like '%date%')
        ;
        create table __gnu as
        select &var_list from &inset_name
        ;
      /* end cb additions */
    quit ;
      data __gnu ;
        set __gnu (obs = &obs_lim keep = &dat_array) ;
        array d &dat_array ;
        do i = 1 to dim(d) ;
          if n(d{i}) then maybe_age = %calcage(bdtvar = d{i}, refdate = "&sysdate9."d) ;
          if maybe_age ge &eldest_age then do ;
            badvar = vname(d{i}) ;
            badvalue = d{i} ;
            output ;
          end ;
          keep badvar badvalue maybe_age ;
        end ;
      run ;
      proc sql outobs = 30 nowarn ;
        insert into phi_warnings(dset, variable, warning)
        select distinct "&inset_name", badvar, "If this is a date, at least one value is " || compress(put(maybe_age, best.)) || " years ago, which is older than &eldest_age..  " ||
        "If this date applies to a person, the record is probably PHI."
        from __gnu ;
        drop table __gnu ;
      quit ;
    %end ;
    %else %do ;
      %put No obvious date variables found in &inset_name.--skipping age checks. ;
    %end ;
  %mend check_vars_for_oldsters ;

  proc contents noprint data = &inset_name out = these_vars ;
  run ;

  proc sql noprint ;
    create table phi_warnings (dset char(50), variable char(256), label char(256), warning char(200)) ;

    %check_varname(regx = mrn|hrn                                               , msg = %str(Name suggests this var may be an MRN, which should never move across sites.)) ;
    %check_varname(regx = birth_date|BirthDate|DOB|BDate                        , msg = %str(Name suggests this var may be a date of birth.)) ;
    %check_varname(regx = SSN|SocialSecurityNumber|social_security_number|socsec, msg = %str(Name suggests this var may be a social security number.)) ;

    %if %symexist(locally_forbidden_varnames) %then %do ;
      %check_varname(regx = &locally_forbidden_varnames, msg = %str(May be on the locally defined list of variables not allowed to be sent to other sites.)) ;
    %end ;

  quit ;

  %check_vars_for_mrn(obs_lim = &obs_lim) ;
  %check_vars_for_oldsters(obs_lim = &obs_lim, eldest_age = &eldest_age) ;

  title3 "WARNINGS for dataset &inset_name:" ;

  proc sql noprint ;
    select count(*) as num_warns into :num_warns from phi_warnings ;

    %if &num_warns = 0 %then %do ;
      reset print outobs = 5 NOWARN ;
      select "No obvious PHI-like data elements in &inset_name--BUT PLEASE INSPECT THE CONTENTS AND PRINTs TO FOLLOW" as x label = "No warnings for &inset_name"
      from &inset_name
      ;
      %do i = 1 %to 5 ;
        %put No obvious phi-like data elements in &inset_name.  BUT PLEASE INSPECT THE CONTENTS AND PRINTs CAREFULLY TO MAKE SURE OF THIS! ;
      %end ;
    %end ;
    %else %do ;
      reset print ;
      select variable, warning from phi_warnings
      order by variable, warning
      ;
      quit ;
    %end ;
    title3 "Dataset &inset_name" ;
    proc contents data = &inset_name varnum ;
    run ;
  /*
    proc print data = &inset_name (obs = 20) ;
    run ;
  */
    ** TODO: make the print print out recs that trip the value warnings. ;
    proc sql number ;
      select *
      from &inset_name (obs = 20)
      ;
    quit ;

  quit ;

  %RemoveDset(dset = __sub_dset) ;
  %RemoveDset(dset = possible_bad_vars) ;
  %RemoveDset(dset = phi_warnings) ;
  %RemoveDset(dset = these_vars) ;

%mend check_dataset ;

%macro detect_phi(transfer_lib, obs_lim = max, eldest_age = 89) ;

  %put ;
  %put ;
  %put ============================================================== ;
  %put ;
  %put Macro detect_phi: ;
  %put ;
  %put Checking all datasets found in %sysfunc(pathname(&transfer_lib)) for the following signs of PHI: ;
  %put   - Variable names signifying sensitive items like 'MRN', 'birth_date', 'SSN' and so forth. ;
  %if %symexist(locally_forbidden_varnames) %then %do ;
    %put   - Variable names on the list defined in the standard macro variable locally_forbidden_varnames (here those names are: &locally_forbidden_varnames). ;
  %end ;
  %put   - Contents of CHARACTER variables that match the pattern given in the standard macro variable mrn_regex (here that var is &mrn_regex) ;
  %put     Please note that numeric variables ARE NOT CHECKED FOR MRN-LIKE CONTENT. ;
  %put   - The contents of date variables (as divined by their formats) for values that, if they were DOBs, would indicate a person older than &eldest_age years. ;
  %put ;
  %put THIS IS BETA SOFTWARE-PLEASE SCRUTINIZE THE RESULTS AND REPORT PROBLEMS TO pardee.r@ghc.org. ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put THIS MACRO IS NOT A SUBSTITUTE FOR HUMAN INSPECTION AND THOUGHT--PLEASE CAREFULLY INSPECT ALL VARIABLES--WHETHER ;
  %put OR NOT THEY TRIP A WARNING--TO MAKE SURE THE DATA COMPORTS WITH YOUR DATA SHARING AGREEMENT!!! ;
  %put ;
  %put ;
  %put ============================================================== ;
  %put ;
  %put ;

  title1 "PHI-Detection Report for the datasets in %sysfunc(pathname(&transfer_lib))." ;
  title2 "please inspect all output carefully to make sure it comports with your data sharing agreement!!!" ;

  proc sql noprint ;
    ** describe table dictionary.tables ;

    select trim(libname) || '.' || memname as dset
    into   :d1-:d999
    from dictionary.tables
    where libname = "%upcase(&transfer_lib)" AND
          memtype = 'DATA'
    ;
    %local num_dsets ;
    %let num_dsets = &sqlobs ;
  quit ;

  %local i ;

  %if &num_dsets = 0 %then %do i = 1 %to 10 ;
    %put ERROR: NO DATASETS FOUND IN &transfer_lib!!!! ;
  %end ;

  %do i = 1 %to &num_dsets ;
    %put about to check &&d&i ;
    %check_dataset(dset = &&d&i, obs_lim = &obs_lim, eldest_age = &eldest_age) ;
  %end ;

%mend detect_phi ;

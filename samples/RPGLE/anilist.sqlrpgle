**free

ctl-opt main(main);
ctl-opt option(*srcstmt:*noDebugIO:*nounref) dftActGrp(*no);
ctl-opt datfmt(*iso) timfmt(*iso);


dcl-f ALDSPF workStn(*ext) indDs(dspf) usropn;

dcl-ds dspf qualified;
  exit    ind pos(3);
  refresh ind pos(5);
  cancel  ind pos(12);
end-ds;

dcl-s reqUrl    char(64);
dcl-s reqHeader char(128);

dcl-pr main extPgm('ANILIST') end-pr;


dcl-proc main;
  reqUrl = 'https://graphql.anilist.co';
  reqHeader = '<httpHeader><header name="Content-Type"' + 
    ' value="application/json"/></httpHeader>';

  monitor;
    open ALDSPF;
  on-error *file;
    dsply ('Could not open display file ALDSPF');
    return;
  endmon;
  dspfLoop ();

  on-exit;
    resetDspf ();
    close *ALL;
end-proc;


dcl-proc dspfLoop;
  monitor;
    doU (dspf.exit);
      exfmt ALDR001;

      if (dspf.cancel or dspf.exit);
        leave;
      elseif (dspf.refresh);
        clear ALDR001;
      elseif (ALDIUSRNM <> *BLANK);
        getUser ();
        getMediaLists ();
        write ALDR001;
      endif;
      
    enddo;
  on-error;
    ALDOERROR = 'Error in dspfLoop()';
    write ALDR001;
  endmon;
end-proc;


dcl-proc getMediaLists;

  dcl-ds listCounts qualified;
    completed varchar(4);
    dropped   varchar(4);
    planned   varchar(4);
    current   varchar(4);
    paused    varchar(4);
    repeat    varchar(4);
  end-ds;
  
  dcl-s reqBody varchar(256);
  reqBody = '{"query": "{MediaListCollection(userId:' + ALDOID + 
    ', type:ANIME){lists{name entries {id}}}}"}';

  monitor;
    // Get media list entries, count them by type, 
    //   and pivot the resultset for easy insert to DS
    exec SQL
      with ml as (
        select 
          upper(name) as name,
          id
        from json_table(
          Systools.HttpPostClob(:reqUrl, :reqHeader, :reqBody),
          '$.data.MediaListCollection.lists[*]' columns(
            name char(32) path '$.name',
            nested '$.entries[*]' columns(
              id char(32) path '$.id'
            )
          )
        )
      )
      select
        (select count(*) from ml where name = 'COMPLETED') as completed,
        (select count(*) from ml where name = 'DROPPED'  ) as dropped,
        (select count(*) from ml where name = 'PLANNING' ) as planned,
        (select count(*) from ml where name = 'WATCHING' ) as current,
        (select count(*) from ml where name = 'PAUSED'   ) as paused,
        (select count(*) from ml where name = 'REPEATING') as repeat
      into :listCounts
      from ml
      limit 1;

    ALDOLCOMP = listCounts.completed;
    ALDOLDROP = listCounts.dropped;
    ALDOLPLAN = listCounts.planned;
    ALDOLCURR = listCounts.current;
    ALDOLPAUS = listCounts.paused;
    ALDOLREPT = listCounts.repeat;
  on-error;
    ALDOERROR = 'Error in getMediaLists ()';
  endmon;
  return;
end-proc;


dcl-proc getUser;

  dcl-ds user qualified;
    id       varchar(10);
    username varchar(18);
    url      varchar(32);
    hours    varchar(10);
  end-ds;

  dcl-s reqBody varchar(256);
  reqBody = '{"query": "{User(search:\"' + %trim(ALDIUSRNM) +
    '\"){id name siteUrl stats{watchedTime}}}"}';
  
  monitor;
    exec SQL
      select
        id,
        username,
        url,
        (minutes / 60) as hours 
      into :user
      from json_table(
        Systools.HttpPostClob(:reqUrl, :reqHeader, :reqBody),
      '$.data.User'
      columns(
        id       varchar(10) path '$.id',
        username varchar(20) path '$.name',
        url      varchar(32) path '$.siteUrl',
        minutes  varchar(10) path '$.stats.watchedTime'
      )
    );
    
    ALDOID = user.id;
    ALDONAME = user.username;
    ALDOHOURS = user.hours;
    ALDOURL = user.url;
  on-error;
    ALDOERROR = 'Error in getUser()';
  endmon;

  return;
end-proc;


dcl-proc resetDspf;
  clear ALDR001;
  dspf.exit = *OFF;
  dspf.cancel = *OFF;
end-proc;


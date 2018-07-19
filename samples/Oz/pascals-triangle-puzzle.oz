%% to compile : ozc -x <file.oz>
functor

import
  System Application FD Search
define

  proc{Quest Root Rules}

    proc{Limit Rc Ls}
      case Ls of nil then skip
      [] X|Xs then
        {Limit Rc Xs}
        case X of N#V then
          Rc.N =: V
        [] N1#N2#N3 then
          Rc.N1 =: Rc.N2 + Rc.N3
        end
      end
    end

    proc {Pyramid R}
      {FD.tuple solution 15 0#FD.sup R}  %% non-negative integers domain
  %%          01      , pyramid format
  %%        02  03
  %%      04  05  06
  %%    07  08  09  10
  %%  11  12  13  14  15
      R.1 =: R.2 + R.3     %% constraints of Pyramid of numbers
      R.2 =: R.4 + R.5
      R.3 =: R.5 + R.6
      R.4 =: R.7 + R.8
      R.5 =: R.8 + R.9
      R.6 =: R.9 + R.10
      R.7 =: R.11 + R.12
      R.8 =: R.12 + R.13
      R.9 =: R.13 + R.14
      R.10 =: R.14 + R.15

      {Limit R Rules}      %% additional constraints

      {FD.distribute ff R}
    end
  in
    {Search.base.one Pyramid Root} %% search for solution
  end

  local
    Root R
  in
    {Quest Root [1#151 4#40 12#11 14#4 13#11#15]} %% supply additional constraint rules
    if {Length Root} >= 1 then
      R = Root.1
      {For 1 15 1
        proc{$ I}
          if {Member I [1 3 6 10]} then
            {System.printInfo R.I#'\n'}
          else
            {System.printInfo R.I#' '}
          end
        end
      }
    else
      {System.showInfo 'No solution found.'}
    end
  end

  {Application.exit 0}
end

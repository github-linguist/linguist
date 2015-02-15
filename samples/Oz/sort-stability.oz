declare
  Cities = ['UK'#'London'
            'US'#'New York'
            'US'#'Birmingham'
            'UK'#'Birmingham']
in
  %% sort by city; stable because '=<' is reflexiv
  {Show {Sort Cities fun {$ A B} A.2 =< B.2 end}}

  %% sort by country; NOT stable because '<' is not reflexiv
  {Show {Sort Cities fun {$ A B} A.1 < B.1 end}}

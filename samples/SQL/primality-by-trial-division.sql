declare @number int
set @number = 514229 -- number to check

;with cte(number) as
(
 select 2
 union all
 select number+1
 from cte
 where number+1 < @number
)
select
      cast(@number as varchar(100)) +
      case
          when exists
				  (
					select *
					from
					(
						select number, @number % number modNumber
						from cte
					) tmp
					where tmp.modNumber = 0
				  )
				          then ' is composite'
		  else
						 ' is prime'
	  end primalityTest
option (maxrecursion 0)

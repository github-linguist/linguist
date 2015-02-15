declare
  fun {Const X}
     fun {$ _} X end
  end

  fun {Now}
     {Int.toFloat {Property.get 'time.total'}} / 1000.0
  end

  class Integrator from Time.repeat
     attr
        k:{Const 0.0}
        s:0.0
        t1 k_t1
        t2 k_t2

     meth init(SampleIntervalMS)
        t1 := {Now}
        k_t1 := {@k @t1}
        {self setRepAll(action:Update
                        delay:SampleIntervalMS)}
        thread
           {self go}
        end
     end

     meth input(K)
        k := K
     end

     meth output($)
        @s
     end

     meth Update
        t2 := {Now}
        k_t2 := {@k @t2}
        s := @s + (@k_t1 + @k_t2) * (@t2 - @t1) / 2.0
        t1 := @t2
        k_t1 := @k_t2
     end
  end

  Pi = 3.14159265
  F = 0.5

  I = {New Integrator init(10)}
in
  {I input(fun {$ T}
              {Sin 2.0 * Pi * F * T}
           end)}

  {Delay 2000} %% ms

  {I input({Const 0.0})}

  {Delay 500} %% ms

  {Show {I output($)}}
  {I stop}

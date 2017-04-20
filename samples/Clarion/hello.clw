  MEMBER()
  INCLUDE('HelloClass.inc'),ONCE
  MAP
  END

HelloClass.Construct              PROCEDURE
  CODE
HelloClass.Destruct               PROCEDURE() !,VIRTUAL
  CODE
HelloClass.SayHello               PROCEDURE
  CODE
  MESSAGE('Hello World!')
